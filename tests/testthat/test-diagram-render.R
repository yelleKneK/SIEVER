# The base graphics renderer of the path diagram.
#
# The renderer returns a record of the primitives it drew (one row per node
# and per edge, with the label, the line type, the arrowheads, the ends of
# the path, and the label box). These tests compare that record with the
# specification, and the specification is checked against lavaan in
# test-diagram-spec.R, so nothing here trusts a picture. The acceptance
# cases, their models, data, and seeds are in helper-diagram-cases.R.

# Draw on a temporary PDF device and return the record of what was drawn.
dr_record <- function(spec, ..., width = NULL, height = NULL) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file,
    width = if (is.null(width)) spec$size$width else width,
    height = if (is.null(height)) spec$size$height + 1.6 else height
  )
  dev <- grDevices::dev.cur()
  on.exit(
    {
      if (dev %in% grDevices::dev.list()) {
        grDevices::dev.off(dev)
      }
      unlink(file)
    },
    add = TRUE
  )
  sieve_diagram_render(spec, sieve_diagram_style(spec, ...))
}

# Is a point on the outline of a node (an ellipse, or a rectangle for an
# observed variable)?
dr_on_outline <- function(node, x, y, tol = 1e-6) {
  dx <- abs(x - node$x) / (node$w / 2)
  dy <- abs(y - node$y) / (node$h / 2)
  r <- if (identical(node$kind, "observed")) max(dx, dy) else sqrt(dx^2 + dy^2)
  abs(r - 1) < tol
}

# Label boxes as xmin, xmax, ymin, ymax.
dr_boxes <- function(edges) {
  cbind(
    edges$label_x - edges$label_w / 2, edges$label_x + edges$label_w / 2,
    edges$label_y - edges$label_h / 2, edges$label_y + edges$label_h / 2
  )
}

# The label the renderer writes for an edge of the specification.
dr_expected_label <- function(e) {
  out <- ifelse(e$derived, paste0("(", e$label_text, ")"), e$label_text)
  out <- ifelse(e$improper & nzchar(out), paste(out, "(!)"), out)
  ifelse(is.na(e$constraint), out, paste0(out, " [", e$constraint, "]"))
}

dr_spec <- function(name) diagram_case_spec(diagram_cases()[[name]])

dr_png_size <- function(file) {
  bytes <- as.integer(readBin(file, "raw", 24L))
  c(width = sum(bytes[17:20] * 256^(3:0)), height = sum(bytes[21:24] * 256^(3:0)))
}

test_that("every acceptance case draws, and what is drawn is the visible part of the specification", {
  skip_if_not_installed("lavaan")
  cases <- diagram_cases()
  expect_true(all(sprintf("C%02d", 1:12) %in% vapply(cases, function(x) x$case, "")))
  for (case in cases[vapply(cases, function(x) x$drawable, NA)]) {
    spec <- diagram_case_spec(case)
    rec <- do.call(dr_record, c(list(spec), case$draw))
    shown <- spec$edges[spec$edges$visible, ]
    seen <- spec$nodes[spec$nodes$visible, ]
    drawn <- rec$edges
    # The drawn edges are the visible edges: each once, none invented.
    expect_identical(drawn$id, shown$id, info = case$name)
    expect_true(all(drawn$drawn), info = case$name)
    expect_identical(drawn$type, shown$type, info = case$name)
    expect_identical(drawn$from, shown$from, info = case$name)
    expect_identical(drawn$to, shown$to, info = case$name)
    expect_identical(rec$nodes$id, seen$id, info = case$name)
    expect_true(all(rec$nodes$drawn), info = case$name)
    # The notation: shape by kind of variable, arrowheads by kind of edge.
    expect_identical(rec$nodes$shape,
      unname(c(
        latent = "ellipse", observed = "rectangle",
        residual = "circle"
      )[seen$kind]),
      info = case$name
    )
    heads <- c(
      loading = 1, regression = 1, residual_path = 1, covariance = 2, variance = 2,
      intercept = 0, threshold = 0
    )
    expect_identical(drawn$heads, unname(heads[shown$type]), info = case$name)
    shape <- c(
      loading = "line", regression = "line", residual_path = "line",
      covariance = "arc", variance = "loop", intercept = "note", threshold = "note"
    )
    expect_identical(drawn$shape, unname(shape[shown$type]), info = case$name)
    # Fixed, implied, and estimated status come from the specification.
    parameter <- !is.na(shown$par) & !shown$type %in% c("intercept", "threshold")
    status <- ifelse(shown$fixed, "fixed", ifelse(shown$derived, "derived", "free"))
    expect_identical(drawn$line[parameter], status[parameter], info = case$name)
    expect_true(all(drawn$line[shown$type == "residual_path"] == "free"), info = case$name)
    # The label is the label of the specification, sign and all.
    lined <- !shown$type %in% c("intercept", "threshold")
    expect_identical(drawn$label_text, shown$label_text, info = case$name)
    expect_identical(drawn$label[lined], dr_expected_label(shown)[lined], info = case$name)
    expect_identical(drawn$flagged, shown$improper, info = case$name)
    # Node labels are whole, at the type size of the layout.
    expect_identical(rec$nodes$label[seen$kind != "residual"],
      seen$label[seen$kind != "residual"],
      info = case$name
    )
    named <- rec$nodes$shape != "circle"
    expect_true(all(rec$nodes$pt[named] > 9 & rec$nodes$pt[named] <= 10 + 1e-9),
      info = case$name
    )
    # The letter of a residual term is larger, and still inside its circle.
    expect_true(
      all(rec$nodes$pt[!named] > 10 &
        rec$nodes$pt[!named] <= sieve_diagram_look()$greek_pt + 1e-9),
      info = case$name
    )

    # Every path starts and ends on the outline of its two nodes.
    for (r in which(drawn$shape != "note")) {
      from <- seen[match(drawn$from[r], seen$id), ]
      to <- seen[match(drawn$to[r], seen$id), ]
      expect_true(dr_on_outline(from, drawn$x0[r], drawn$y0[r]),
        info = paste(case$name, drawn$id[r], "start")
      )
      expect_true(dr_on_outline(to, drawn$x1[r], drawn$y1[r]),
        info = paste(case$name, drawn$id[r], "end")
      )
    }
    # Nothing is clipped: nodes and labels are inside the canvas, which is
    # inside the plot region.
    labeled <- drawn[nzchar(drawn$label), ]
    box <- dr_boxes(labeled)
    expect_true(
      all(box[, 1] >= rec$canvas$xlim[1] & box[, 2] <= rec$canvas$xlim[2] &
        box[, 3] >= rec$canvas$ylim[1] & box[, 4] <= rec$canvas$ylim[2]),
      info = case$name
    )
    expect_true(all(seen$x - seen$w / 2 >= rec$canvas$xlim[1] &
      seen$x + seen$w / 2 <= rec$canvas$xlim[2] &
      seen$y - seen$h / 2 >= rec$canvas$ylim[1] &
      seen$y + seen$h / 2 <= rec$canvas$ylim[2]), info = case$name)
    expect_true(rec$canvas$xlim[1] >= rec$xlim[1] - 1e-9 &&
      rec$canvas$xlim[2] <= rec$xlim[2] + 1e-9 &&
      rec$canvas$ylim[1] >= rec$ylim[1] - 1e-9 &&
      rec$canvas$ylim[2] <= rec$ylim[2] + 1e-9, info = case$name)
    # No label sits on a node, and no two labels overlap.
    on_node <- 0L
    for (i in seq_len(nrow(labeled))) {
      gx <- rep(c(box[i, 1], labeled$label_x[i], box[i, 2]), 3)
      gy <- rep(c(box[i, 3], labeled$label_y[i], box[i, 4]), each = 3)
      for (j in seq_len(nrow(seen))) {
        on_node <- on_node + any(sieve_diagram_inside(seen[j, ], gx, gy))
      }
    }
    expect_identical(on_node, 0L, info = case$name)
    clash <- 0L
    for (i in seq_len(nrow(labeled) - 1L)) {
      j <- (i + 1L):nrow(labeled)
      clash <- clash + sum(box[i, 1] < box[j, 2] - 1e-6 & box[j, 1] < box[i, 2] - 1e-6 &
        box[i, 3] < box[j, 4] - 1e-6 & box[j, 3] < box[i, 4] - 1e-6)
    }
    expect_identical(clash, 0L, info = case$name)
  }
})

test_that("sieve_draw_diagram() draws on the current device and returns the specification", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C02_two_factors_unit_variance")
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file, width = 9, height = 7)
  dev <- grDevices::dev.cur()
  withr::defer({
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    unlink(file)
  })
  graphics::plot.new()
  devices <- grDevices::dev.list()
  before <- graphics::par(no.readonly = TRUE)
  out <- withVisible(sieve_draw_diagram(spec))
  expect_false(out$visible)
  expect_identical(out$value, spec)
  # The device is still the current one, and no other was opened.
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(grDevices::dev.cur(), dev)
  # Every setting the renderer changed is restored, the coordinate system
  # of the plot included: the device is left as it was found.
  after <- graphics::par(no.readonly = TRUE)
  expect_identical(after, before)
  # Options are checked, and an unknown argument is not swallowed.
  expect_error(sieve_draw_diagram(spec, colour = "red"), "no further arguments")
  expect_error(sieve_draw_diagram(spec, "nd", FALSE, 1, TRUE, "serif"), "no further arguments")
  # A misspelled or abbreviated argument is not matched to a real one.
  expect_error(sieve_draw_diagram(spec, monochrom = TRUE), "no further arguments")
  expect_error(sieve_draw_diagram(spec, pal = "nd"), "no further arguments")
  expect_error(plot(spec, show_cap = FALSE), "no further arguments")
  expect_error(sieve_draw_diagram(spec, cex = 0), "`cex` must be")
  expect_error(sieve_draw_diagram(spec, monochrome = NA), "`monochrome` must be")
  expect_error(sieve_draw_diagram(spec, show_caption = "yes"), "`show_caption` must be")
  expect_error(sieve_draw_diagram(spec, palette = "no-such-palette"), "`palette` must be")
  expect_error(sieve_draw_diagram(spec, family = NA_character_), "`family` must be")
  expect_identical(graphics::par(no.readonly = TRUE), after)
  grDevices::dev.off(dev)
  expect_gt(file.size(file), 0)
})

test_that("the covariance toggles change the drawn edge set and nothing else", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C02_two_factors_unit_variance")
  rec <- dr_record(spec)
  arc <- rec$edges[rec$edges$type == "covariance", ]
  expect_identical(arc$id, "Visual~~Textual")
  expect_identical(c(arc$from, arc$to), c("Visual", "Textual"))
  expect_identical(arc$heads, 2)
  # One arc: exactly the curve the specification routes, above the factors.
  e <- spec$edges[spec$edges$id == "Visual~~Textual", ]
  a <- spec$nodes[spec$nodes$id == "Visual", ]
  b <- spec$nodes[spec$nodes$id == "Textual", ]
  t <- seq(0, 1, length.out = 20001)
  d <- c(b$x - a$x, b$y - a$y)
  top <- c(a$x + b$x, a$y + b$y) / 2 + 2 * e$curve * c(-d[2], d[1]) / sqrt(sum(d^2))
  bx <- (1 - t)^2 * a$x + 2 * t * (1 - t) * top[1] + t^2 * b$x
  by <- (1 - t)^2 * a$y + 2 * t * (1 - t) * top[2] + t^2 * b$y
  expect_lt(min(sqrt((bx - arc$x0)^2 + (by - arc$y0)^2)), 1e-3)
  expect_lt(min(sqrt((bx - arc$x1)^2 + (by - arc$y1)^2)), 1e-3)
  expect_gt(arc$label_y, max(a$y + a$h / 2, b$y + b$h / 2))
  expect_identical(arc$label, formatC(e$std, format = "f", digits = 2))

  hidden <- sieve_diagram_update(spec, covariances = FALSE)
  rec_hidden <- dr_record(hidden)
  expect_identical(setdiff(rec$edges$id, rec_hidden$edges$id), "Visual~~Textual")
  expect_identical(setdiff(rec_hidden$edges$id, rec$edges$id), character(0))
  # Nothing else moved.
  same <- match(rec_hidden$edges$id, rec$edges$id)
  for (col in c("x0", "y0", "x1", "y1", "label_x", "label_y", "label")) {
    expect_identical(rec_hidden$edges[[col]], rec$edges[[col]][same], info = col)
  }
  expect_identical(rec_hidden$nodes, rec$nodes)

  # C09: the residual covariance joins the two unique terms, and the factor
  # covariance toggle neither hides nor adds it.
  res_id <- "t10_addition~~t12_counting_groups_of_dots"
  full <- dr_record(dr_spec("C09_residual_covariance"))
  off <- dr_record(dr_spec("C09_residual_covariance_factor_covariances_hidden"))
  joined <- full$edges[full$edges$id == res_id, ]
  expect_identical(
    c(joined$from, joined$to),
    c("e.t10_addition", "e.t12_counting_groups_of_dots")
  )
  expect_identical(
    full$nodes$shape[match(c(joined$from, joined$to), full$nodes$id)],
    c("circle", "circle")
  )
  expect_identical(sum(full$edges$type == "covariance"), 4L)
  expect_identical(off$edges$id[off$edges$type == "covariance"], res_id)
  expect_identical(off$edges[off$edges$id == res_id, c("x0", "y0", "x1", "y1", "label")],
    joined[, c("x0", "y0", "x1", "y1", "label")],
    ignore_attr = TRUE
  )
  only_factor <- sieve_diagram_update(dr_spec("C09_residual_covariance"),
    residual_covariances = FALSE
  )
  expect_false(res_id %in% dr_record(only_factor)$edges$id)
  expect_identical(sum(dr_record(only_factor)$edges$type == "covariance"), 3L)
  # With the residual terms hidden, the covariance of residuals leaves the
  # figure with them (drawn between the two indicators it would read as a
  # correlation of the indicators, which it is not), and the caption counts
  # it; the factor covariances stay.
  bare_spec <- sieve_diagram_update(dr_spec("C09_residual_covariance"), residuals = FALSE)
  bare <- dr_record(bare_spec)
  expect_false(res_id %in% bare$edges$id)
  expect_false(any(bare$nodes$shape == "circle"))
  expect_identical(bare_spec$edges$hidden_reason[bare_spec$edges$id == res_id], "residuals")
  expect_identical(sum(bare$edges$type == "covariance"), 3L)
  expect_match(bare_spec$meta$caption, "residual covariance", fixed = TRUE)
})

test_that("fixed, implied, and improper values are told apart without color", {
  skip_if_not_installed("lavaan")
  # C03: a marker loading is drawn as fixed in both metrics; only in the
  # metric it was fixed in is it the constant 1.
  raw <- dr_record(dr_spec("C03_two_factors_marker_unstandardized"))$edges
  std <- dr_record(dr_spec("C03_two_factors_marker_std_all"))$edges
  markers <- c("Visual=~t1_visual_perception", "Textual=~t6_paragraph_comprehension")
  expect_identical(raw$line[match(markers, raw$id)], c("fixed", "fixed"))
  expect_identical(raw$label[match(markers, raw$id)], c("1", "1"))
  expect_identical(std$line[match(markers, std$id)], c("fixed", "fixed"))
  expect_match(std$label[match(markers, std$id)], "^0\\.[0-9]{2}$")
  expect_true(all(raw$line[raw$type == "variance"] == "free"))
  # C02: the factor variances are the fixed ones, the loadings are free.
  unit <- dr_record(dr_spec("C02_two_factors_unit_variance"))$edges
  expect_identical(
    unit$line[unit$id %in% c("Visual~~Visual", "Textual~~Textual")],
    c("fixed", "fixed")
  )
  expect_identical(unit$label[unit$id %in% c("Visual~~Visual", "Textual~~Textual")], c("1", "1"))
  expect_true(all(unit$line[unit$type == "loading"] == "free"))
  expect_false(any(unit$label[unit$type == "loading"] == "1"))

  # C04: no covariance edge unless the fixed zero is asked for, and then
  # it is drawn as fixed.
  expect_false(any(dr_record(dr_spec("C04_orthogonal_factors"))$edges$type == "covariance"))
  zero <- dr_record(dr_spec("C04_orthogonal_factors_fixed_zero_drawn"))$edges
  zero <- zero[zero$type == "covariance", ]
  expect_identical(c(zero$id, zero$line, zero$label), c("Visual~~Textual", "fixed", "0"))

  # C10: implied residual variances are drawn as implied; thresholds are
  # drawn only on request, beyond the unique terms, one line each.
  ord <- dr_record(dr_spec("C10_ordered_wlsmv_delta"))
  loops <- ord$edges[ord$edges$type == "variance" & grepl("^e\\.", ord$edges$from), ]
  expect_identical(nrow(loops), 6L)
  expect_true(all(loops$line == "derived"))
  expect_match(loops$label, "^\\(0\\.[0-9]{2}\\)$")
  expect_false(any(ord$edges$type == "threshold"))
  spec_thr <- dr_spec("C10_ordered_wlsmv_delta_thresholds")
  thr <- dr_record(spec_thr)
  notes <- thr$edges[thr$edges$type == "threshold", ]
  expect_identical(nrow(notes), 18L)
  expect_true(all(notes$drawn))
  expect_match(notes$label, "^tau[1-3] = -?[0-9]\\.[0-9]{2}$")
  expect_identical(sub("^tau[1-3] = ", "", notes$label), notes$label_text)
  unique_y <- thr$nodes$y[match(paste0("e.", notes$from), thr$nodes$id)]
  expect_true(all(notes$label_y < unique_y))
  # The canvas grows to hold them; the diagram itself does not move.
  expect_lt(thr$canvas$ylim[1], spec_thr$size$ylim[1])
  expect_identical(thr$nodes[, c("id", "x", "y")], ord$nodes[, c("id", "x", "y")])
  binary <- dr_record(dr_spec("C10_binary_wlsmv_delta"))$edges
  expect_true(all(binary$line[binary$type == "variance" & grepl("^e\\.", binary$from)] == "derived"))

  # C12: the Heywood variance keeps its sign and is flagged.
  spec_hey <- dr_spec("C12_heywood")
  hey <- dr_record(spec_hey)$edges
  bad <- hey[hey$id == "y1~~y1", ]
  expect_true(bad$flagged)
  # The flag is in the text as well as in the box, so it survives a copy of
  # the labels; the value itself is the value of the specification.
  expect_identical(bad$label_text, spec_hey$edges$label_text[spec_hey$edges$id == "y1~~y1"])
  expect_identical(bad$label, paste(bad$label_text, "(!)"))
  expect_match(bad$label, "^-0\\.[0-9]{2} \\(!\\)$")
  expect_identical(hey$id[hey$flagged], c("f=~y1", "y1~~y1"))
  expect_false(any(grepl("(!)", hey$label[!hey$flagged], fixed = TRUE)))
  # C11: the negative loading keeps its sign and is not flagged.
  sim <- dr_record(dr_spec("C11_long_labels_negative_loading"))
  neg <- sim$edges[sim$edges$id == "trait=~q4", ]
  expect_match(neg$label, "^-0\\.[0-9]{2}$")
  expect_false(neg$flagged)
  expect_identical(neg$line, "free")
  expect_identical(
    sim$nodes$label[sim$nodes$id == "q1"],
    unname(diagram_case_long_labels()[["q1"]])
  )
  # A value that is not known is written as not known.
  unknown <- dr_spec("C01_one_factor")
  unknown$edges$value[1] <- NA_real_
  unknown$edges$label_text[1] <- sieve_diagram_labels(unknown$edges[1, ], 2L)
  expect_identical(dr_record(unknown)$edges$label[1], "NA")
})

test_that("residual terms carry the letter of their role, and levels and regressions are kept apart", {
  skip_if_not_installed("lavaan")
  # C01: unique terms of the indicators of an exogenous factor are delta.
  one <- dr_record(dr_spec("C01_one_factor"))
  expect_identical(unique(one$nodes$label[one$nodes$shape == "circle"]), "delta")
  # C07: first-order factors have disturbances (zeta) that carry their
  # residual variance; their indicators' unique terms are epsilon.
  spec <- dr_spec("C07_higher_order")
  high <- dr_record(spec)
  first <- c("Visual", "Textual", "Speed")
  expect_identical(
    high$nodes$label[match(paste0("e.", first), high$nodes$id)],
    rep("zeta", 3)
  )
  expect_identical(
    unique(high$nodes$label[high$nodes$shape == "circle" &
      !high$nodes$id %in% paste0("e.", first)]),
    "epsilon"
  )
  loops <- high$edges[high$edges$type == "variance", ]
  expect_false(any(loops$from %in% first))
  expect_true(all(paste0("e.", first) %in% loops$from))
  up <- high$edges[high$edges$from == "g" & high$edges$type == "loading", ]
  expect_identical(up$to, first)
  expect_true(all(up$y0 > up$y1))
  # C08: both covariances are drawn, each on the nodes of its class.
  sem_spec <- dr_spec("C08_structural_left_right")
  sem <- dr_record(sem_spec)
  cov <- sem$edges[sem$edges$type == "covariance", ]
  expect_identical(cov$id, c("Speed~~t24_woody_mccall", "Spatial~~Verbal"))
  expect_identical(
    unlist(cov[cov$id == "Spatial~~Verbal", c("from", "to")], use.names = FALSE),
    c("Spatial", "Verbal")
  )
  expect_identical(unlist(cov[cov$id == "Speed~~t24_woody_mccall", c("from", "to")],
    use.names = FALSE
  ), c("e.Speed", "e.t24_woody_mccall"))
  expect_identical(
    sem$nodes$label[match(c("e.Speed", "e.t24_woody_mccall"), sem$nodes$id)],
    c("zeta", "zeta")
  )
  expect_identical(sum(sem$edges$type == "regression"), 4L)
  expect_identical(sum(sem$edges$type == "loading"), 13L)
  caption <- sieve_diagram_figure_caption(sem_spec)
  expect_match(caption, "heavy arrow, regression; light arrow, loading", fixed = TRUE)
  expect_match(caption, "residual covariances, not correlations of the variables", fixed = TRUE)
  expect_match(caption, sem_spec$meta$caption_parts[["scale"]], fixed = TRUE)
  look <- sieve_diagram_look()
  expect_gt(look$lwd[["regression"]], 1.5 * look$lwd[["loading"]])
})

test_that("two opposite regressions are two arrows, never one double-headed arrow", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::sem(
    "t1_visual_perception ~ t2_cubes + t6_paragraph_comprehension
                      t2_cubes ~ t1_visual_perception + t7_sentence",
    sieve_hs9_fixture()$data
  )
  spec <- sieve_diagram_spec(fit, label_mode = "short")
  rec <- dr_record(spec)
  pair <- rec$edges[rec$edges$id %in% c(
    "t1_visual_perception~t2_cubes",
    "t2_cubes~t1_visual_perception"
  ), ]
  expect_identical(nrow(pair), 2L)
  expect_identical(pair$type, c("regression", "regression"))
  expect_identical(pair$heads, c(1, 1))
  expect_identical(pair$from, rev(pair$to))
  # Parallel, apart by the gap of the look, and each label on the outer
  # side of its own line.
  d1 <- c(pair$x1[1] - pair$x0[1], pair$y1[1] - pair$y0[1])
  d2 <- c(pair$x1[2] - pair$x0[2], pair$y1[2] - pair$y0[2])
  expect_equal(sum(d1 * d2) / sqrt(sum(d1^2) * sum(d2^2)), -1)
  normal <- c(-d1[2], d1[1]) / sqrt(sum(d1^2))
  offset <- function(x, y) sum((c(x, y) - c(pair$x0[1], pair$y0[1])) * normal)
  expect_equal(abs(offset(pair$x0[2], pair$y0[2])), sieve_diagram_look()$pair_gap)
  # Each value is on its own line of the pair or on the outer side of it,
  # never between the two lines and never on the other line.
  expect_gte(offset(pair$label_x[1], pair$label_y[1]), -1e-9)
  expect_lte(offset(pair$label_x[2], pair$label_y[2]), offset(pair$x0[2], pair$y0[2]) + 1e-9)
  on_own <- abs(c(
    offset(pair$label_x[1], pair$label_y[1]),
    offset(pair$label_x[2], pair$label_y[2]) - offset(pair$x0[2], pair$y0[2])
  ))
  expect_identical(pair$on_line, on_own < 1e-9)
  expect_false(any(rec$edges$type == "covariance" & rec$edges$from %in% pair$from &
    rec$edges$to %in% pair$from))
})

test_that("the caption states the conventions the figure uses", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C03_two_factors_marker_unstandardized")
  caption <- sieve_diagram_figure_caption(spec)
  for (part in spec$meta$caption_parts) {
    expect_match(caption, part, fixed = TRUE)
  }
  expect_match(caption, "dashed line, parameter fixed in estimation", fixed = TRUE)
  expect_match(caption, "small circle, residual term (delta, a unique term)", fixed = TRUE)
  expect_no_match(caption, "dotted line", fixed = TRUE)
  expect_match(sieve_diagram_figure_caption(dr_spec("C10_ordered_wlsmv_delta")),
    "dotted line with the value in parentheses, implied by the model",
    fixed = TRUE
  )
  expect_match(sieve_diagram_figure_caption(dr_spec("C10_ordered_wlsmv_delta_thresholds")),
    "tau, threshold",
    fixed = TRUE
  )
  expect_match(sieve_diagram_figure_caption(dr_spec("C12_heywood")),
    "boxed value in bold type marked (!), out of range",
    fixed = TRUE
  )
  expect_match(sieve_diagram_figure_caption(dr_spec("C04_orthogonal_factors")),
    "Fixed at zero and not drawn: Visual ~~ Textual.",
    fixed = TRUE
  )
  expect_match(sieve_diagram_figure_caption(dr_spec("C11_no_standard_errors")),
    "uncertainty is unknown",
    fixed = TRUE
  )
  # Short identifiers come with their key; full labels need none.
  short <- dr_spec("C06_five_factors_24_indicators_short_labels")
  expect_match(sieve_diagram_figure_caption(short),
    "Key: V1 = Visual perception; V2 = Cubes (spatial visualization); ",
    fixed = TRUE
  )
  expect_match(sieve_diagram_figure_caption(short),
    "V24 = Woody-McCall mixed fundamentals (arithmetic).",
    fixed = TRUE
  )
  expect_no_match(caption, "Key:", fixed = TRUE)
  rec <- dr_record(spec)
  expect_identical(paste(rec$caption, collapse = " "), caption)
  expect_identical(dr_record(spec, show_caption = FALSE)$caption, character(0))
  expect_false(grepl(intToUtf8(8212L), caption, fixed = TRUE))
})

test_that("color is an accent only, and the monochrome figure carries the same information", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C05_three_factors")
  luminance <- function(col) {
    rgb <- grDevices::col2rgb(col) / 255
    0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  }
  gray <- function(col) {
    rgb <- grDevices::col2rgb(col)
    all(rgb[1, ] == rgb[2, ] & rgb[2, ] == rgb[3, ])
  }
  color <- sieve_diagram_style(spec)
  expect_true(all(luminance(color$fill) > 0.85))
  expect_true(all(color$fill[spec$nodes$kind != "latent"] == "#ffffff"))
  expect_lt(luminance(color$ink), 0.15)
  expect_true(all(luminance(color$border) < 0.5))
  # One outline color per construct, shared with its indicators.
  by_block <- tapply(
    color$border[spec$nodes$kind != "residual"],
    spec$nodes$block[spec$nodes$kind != "residual"],
    function(x) length(unique(x))
  )
  expect_true(all(by_block == 1L))
  expect_identical(length(unique(color$border[spec$nodes$kind == "latent"])), 3L)
  mono <- sieve_diagram_style(spec, monochrome = TRUE)
  expect_true(gray(c(mono$border, mono$fill, mono$line, mono$ink, mono$flag)))
  expect_true(all(mono$fill == "#ffffff"))
  plain <- sieve_diagram_style(spec, palette = NULL)
  expect_true(gray(c(plain$border, plain$fill)))
  own <- sieve_diagram_style(spec, palette = c("#aa0000", "#00aa00", "#0000aa"))
  expect_identical(length(unique(own$border[spec$nodes$kind == "latent"])), 3L)
  for (name in c("okabe_ito", "viridis")) {
    alt <- sieve_diagram_style(spec, palette = name)
    expect_true(all(luminance(alt$border) < 0.5), info = name)
  }
  # What distinguishes the edges does not depend on color: the record of
  # the monochrome drawing is the record of the color drawing.
  expect_identical(dr_record(spec, monochrome = TRUE), dr_record(spec))
  # `cex` scales the text and leaves the geometry alone.
  big <- dr_record(spec, cex = 1.2, show_caption = FALSE)
  base <- dr_record(spec, show_caption = FALSE)
  expect_identical(big$edges[, c("x0", "y0", "x1", "y1")], base$edges[, c("x0", "y0", "x1", "y1")])
  expect_gt(mean(big$edges$label_w), mean(base$edges$label_w))
  expect_true(all(big$nodes$pt[big$nodes$shape != "circle"] <= 12 + 1e-9))
  expect_true(all(big$nodes$pt <= 1.2 * sieve_diagram_look()$greek_pt + 1e-9))
})

test_that("covariance arcs are clearly curved and their arrowheads do not pile up", {
  skip_if_not_installed("lavaan")
  # Two factors: the arc leaves its factors at a clear angle and rises
  # above them (a flat arc reads as a straight line).
  spec <- dr_spec("C02_two_factors_unit_variance")
  e <- spec$edges[spec$edges$type == "covariance", ]
  a <- spec$nodes[spec$nodes$id == e$from, ]
  b <- spec$nodes[spec$nodes$id == e$to, ]
  chord <- sqrt((b$x - a$x)^2 + (b$y - a$y)^2)
  expect_gt(atan(4 * abs(e$curve) / chord) * 180 / pi, 25)
  expect_gt(abs(e$curve), max(a$h, b$h) / 2 + 0.25)
  # Five factors: ten arcs, four at each factor. Their ends are apart by
  # at least the width of an arrowhead, and each value is at the top of
  # its own arc.
  spec5 <- dr_spec("C06_five_factors_24_indicators_short_labels")
  rec <- dr_record(spec5)
  arcs <- rec$edges[rec$edges$type == "covariance", ]
  expect_identical(nrow(arcs), 10L)
  ends <- data.frame(
    node = c(arcs$from, arcs$to), x = c(arcs$x0, arcs$x1),
    y = c(arcs$y0, arcs$y1)
  )
  width <- sieve_diagram_look()$head$covariance[2]
  for (node in unique(ends$node)) {
    at <- ends[ends$node == node, ]
    expect_identical(nrow(at), 4L)
    expect_gt(min(stats::dist(at[, c("x", "y")])), width, label = paste("ends at", node))
  }
  se <- spec5$edges[match(arcs$id, spec5$edges$id), ]
  na <- spec5$nodes[match(se$from, spec5$nodes$id), ]
  nb <- spec5$nodes[match(se$to, spec5$nodes$id), ]
  top_y <- (na$y + nb$y) / 2 + se$curve * (nb$x - na$x) / sqrt((nb$x - na$x)^2 + (nb$y - na$y)^2)
  expect_true(all(arcs$label_y > top_y & arcs$label_y - top_y < 0.2))
  # Each value is on its own arc or just beside it. A value on the arc
  # (on a white ground) is crossed by it. A value beside the arc clears
  # it by the label gap, measured from the label box to the dense path:
  # never less, so the value does not touch its own arc, and never more
  # than the gap plus the sag of the arc under the box, which is under a
  # hundredth of an inch at these spans, so the value does not float away
  # from its arc. Where along the arc the value sits is chosen by the
  # collision search (the top first, then either side of the top), so
  # that a value never cuts the arc of another pair; the dense-path check
  # below tests that.
  paths <- sieve_diagram_paths(spec5)
  ids <- spec5$edges$id[spec5$edges$visible]
  box <- dr_boxes(arcs)
  gap <- sieve_diagram_look()$label_gap
  for (i in seq_len(nrow(arcs))) {
    p <- paths$path[[match(arcs$id[i], ids)]]
    own <- cbind(p[-nrow(p), , drop = FALSE], p[-1L, , drop = FALSE])
    if (isTRUE(arcs$on_line[i])) {
      expect_true(any(sieve_diagram_crosses(own, box[i, ])), info = arcs$id[i])
    } else {
      expect_false(any(sieve_diagram_crosses(own, box[i, ])), info = arcs$id[i])
      corners <- expand.grid(x = box[i, 1:2], y = box[i, 3:4])
      to_path <- min(vapply(seq_len(nrow(corners)), function(q) {
        min(sieve_diagram_distance(own, c(corners$x[q], corners$y[q])))
      }, numeric(1)))
      to_box <- min(sqrt((p[, 1] - pmin(pmax(p[, 1], box[i, 1]), box[i, 2]))^2 +
        (p[, 2] - pmin(pmax(p[, 2], box[i, 3]), box[i, 4]))^2))
      clearance <- min(to_path, to_box)
      expect_gte(clearance, gap - 1e-6, label = paste("clearance of", arcs$id[i]))
      expect_lte(clearance, gap + 0.01, label = paste("clearance of", arcs$id[i]))
    }
  }
  expect_true(all(arcs$cost == 0))
  # A longer arc is higher than the arcs it spans.
  span <- abs(match(se$to, spec5$nodes$id) - match(se$from, spec5$nodes$id))
  expect_true(all(tapply(abs(se$curve), span, min)[-1] > tapply(abs(se$curve), span, max)[-4]))
})

test_that("loading labels line up, and a loading arrives at the middle of the facing side", {
  skip_if_not_installed("lavaan")
  rec <- dr_record(dr_spec("C05_three_factors"))
  load <- rec$edges[rec$edges$type == "loading", ]
  # Indicators in a row: one height for all the labels of a factor, each
  # beside its line, and the arrowhead at the middle of the top side.
  expect_true(all(tapply(load$label_y, load$from, function(y) diff(range(y))) < 1e-9))
  to <- rec$nodes[match(load$to, rec$nodes$id), ]
  expect_equal(load$x1, to$x)
  expect_equal(load$y1, to$y + to$h / 2)
  line_x <- load$x0 + (load$label_y - load$y0) / (load$y1 - load$y0) * (load$x1 - load$x0)
  expect_true(all(abs(load$label_x - line_x) > load$label_w / 2))
  # Indicators in a column: the arrowhead at the middle of the near side,
  # and the label on its line.
  side <- dr_record(dr_spec("C06_five_factors_24_indicators_left_right"))
  load <- side$edges[side$edges$type == "loading", ]
  to <- side$nodes[match(load$to, side$nodes$id), ]
  expect_equal(load$x1, to$x - to$w / 2)
  expect_equal(load$y1, to$y)
  line_y <- load$y0 + (load$label_x - load$x0) / (load$x1 - load$x0) * (load$y1 - load$y0)
  expect_equal(load$label_y, line_y)
  expect_true(all(tapply(load$label_x, load$from, function(x) diff(range(x))) < 1e-9))
  # Variance loops: on the factor above it, on a unique term below it.
  loops <- rec$edges[rec$edges$type == "variance", ]
  on <- rec$nodes[match(loops$from, rec$nodes$id), ]
  expect_true(all(loops$label_y[on$shape == "ellipse"] > on$y[on$shape == "ellipse"]))
  expect_true(all(loops$label_y[on$shape == "circle"] < on$y[on$shape == "circle"]))
})

test_that("export writes each format at the size of the specification", {
  skip_if_not_installed("lavaan")
  spec <- sieve_diagram_update(dr_spec("C02_two_factors_unit_variance"), analysis_id = "A-17")
  dir <- withr::local_tempdir()
  devices <- grDevices::dev.list()

  pdf_file <- file.path(dir, "figure.pdf")
  out <- withVisible(sieve_export_diagram(spec, pdf_file))
  expect_false(out$visible)
  pdf <- out$value
  expect_identical(pdf$format, "pdf")
  expect_identical(pdf$file, normalizePath(pdf_file, winslash = "/"))
  expect_gt(file.size(pdf_file), 1000)
  expect_identical(rawToChar(readBin(pdf_file, "raw", 4L)), "%PDF")
  # The default size is the recommended size plus the room of the caption.
  expect_equal(pdf$width, spec$size$width)
  expect_gt(pdf$height, spec$size$height)
  expect_lt(pdf$height, spec$size$height + 2)
  page <- rawToChar(grepRaw("/MediaBox \\[0 0 [0-9.]+ [0-9.]+\\]",
    readBin(
      pdf_file,
      "raw",
      file.size(pdf_file)
    ),
    value = TRUE
  ))
  page <- as.numeric(strsplit(gsub("[^0-9. ]", "", sub("/MediaBox \\[0 0 ", "", page)), " ")[[1]])
  expect_equal(page / 72, c(pdf$width, pdf$height), tolerance = 0.01)
  expect_true(is.na(pdf$dpi))
  # Provenance travels with the file.
  expect_identical(pdf$analysis_id, "A-17")
  expect_identical(pdf$status, "ok")
  expect_identical(pdf$model, spec$meta$model)
  expect_identical(pdf$standardized, "std.all")
  expect_identical(pdf$versions, spec$meta$versions)
  expect_identical(pdf$versions$lavaan, as.character(utils::packageVersion("lavaan")))
  expect_identical(pdf$code, sieve_diagram_code(spec))
  expect_identical(pdf$options, spec$meta$options)
  expect_match(pdf$caption, "Analysis A-17.", fixed = TRUE)
  expect_identical(pdf$drawn$edges$id, spec$edges$id[spec$edges$visible])
  expect_true(all(pdf$drawn$edges$drawn))

  png_file <- file.path(dir, "figure.png")
  png <- sieve_export_diagram(spec, png_file, dpi = 120)
  expect_identical(png$format, "png")
  expect_identical(png$dpi, 120)
  expect_identical(
    as.integer(readBin(png_file, "raw", 8L)),
    c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)
  )
  expect_equal(unname(dr_png_size(png_file)), round(c(png$width, png$height) * 120),
    tolerance = 1
  )
  # A requested width keeps the proportions of the figure.
  half <- sieve_export_diagram(spec, png_file, width = png$width / 2, dpi = 100)
  expect_equal(half$height, png$height / 2)
  expect_equal(unname(dr_png_size(png_file)), round(c(half$width, half$height) * 100),
    tolerance = 1
  )
  expect_equal(half$drawn$scale, 0.5)
  expect_identical(half$drawn$edges$label, png$drawn$edges$label)
  box <- sieve_export_diagram(spec, png_file, width = 5, height = 5, dpi = 72, show_caption = FALSE)
  expect_identical(c(box$width, box$height), c(5, 5))
  expect_null(box$caption)
  expect_identical(box$drawn$caption, character(0))
  bare <- sieve_export_diagram(spec, png_file, dpi = 72, show_caption = FALSE)
  expect_equal(c(bare$width, bare$height), c(spec$size$width, spec$size$height))

  # The format follows the file name unless it is given.
  named <- sieve_export_diagram(spec, file.path(dir, "by_name.PNG"), dpi = 72)
  expect_identical(named$format, "png")
  forced <- sieve_export_diagram(spec, file.path(dir, "figure.out"), format = "png", dpi = 72)
  expect_identical(forced$format, "png")
  other <- sieve_export_diagram(spec, file.path(dir, "figure.fig"))
  expect_identical(other$format, "pdf")
  expect_identical(rawToChar(readBin(other$file, "raw", 4L)), "%PDF")

  svg_file <- file.path(dir, "figure.svg")
  svg <- tryCatch(sieve_export_diagram(spec, svg_file), sieve_diagram_error = function(e) e)
  if (inherits(svg, "sieve_diagram_error")) {
    # Without svglite and without working cairo support there is no SVG
    # device; the export must say so and write nothing.
    expect_identical(svg$reason, "device")
    expect_false(file.exists(svg_file))
    expect_false(sieve_have("svglite"))
  } else {
    expect_identical(svg$format, "svg")
    text <- readLines(svg_file, warn = FALSE)
    expect_true(any(grepl("<svg", text, fixed = TRUE)))
    expect_gt(file.size(svg_file), 1000)
    expect_equal(c(svg$width, svg$height), c(pdf$width, pdf$height))
    if (identical(svg$device, "svglite")) {
      # The text stays text, so every label can be found in the file.
      all_text <- paste(text, collapse = "\n")
      for (label in c("Visual", "Textual", unique(svg$drawn$edges$label[nzchar(svg$drawn$edges$label)]))) {
        expect_match(all_text, paste0(">", label, "<"), fixed = TRUE)
      }
    }
    # The three formats show the same nodes, edges, and labels.
    same <- c("id", "type", "from", "to", "shape", "heads", "line", "label", "flagged", "drawn")
    expect_identical(svg$drawn$edges[same], pdf$drawn$edges[same])
    expect_identical(png$drawn$edges[same], pdf$drawn$edges[same])
    expect_identical(
      svg$drawn$nodes[c("id", "shape", "label")],
      pdf$drawn$nodes[c("id", "shape", "label")]
    )
    expect_identical(svg$drawn$caption, pdf$drawn$caption)
  }
  # Each export closed the device it opened, and only that one.
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(list.files(tempdir(), pattern = "^sieve_diagram_"), character(0))

  # An export leaves the device the caller was drawing on current.
  mine <- tempfile(fileext = ".pdf")
  grDevices::pdf(mine)
  dev <- grDevices::dev.cur()
  withr::defer({
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    unlink(mine)
  })
  graphics::plot.new()
  par_before <- graphics::par(no.readonly = TRUE)
  sieve_export_diagram(spec, pdf_file)
  expect_identical(grDevices::dev.cur(), dev)
  expect_identical(grDevices::dev.list(), c(devices, dev))
  expect_identical(graphics::par(no.readonly = TRUE), par_before)
})

test_that("a specification that cannot be drawn is refused, by the renderer and by the export", {
  skip_if_not_installed("lavaan")
  cases <- diagram_cases()
  dir <- withr::local_tempdir()
  files <- list.files(getwd(), all.files = TRUE)
  # With no device open, a refusal must not open one (in a script that
  # would be a stray Rplots.pdf).
  expect_null(grDevices::dev.list())
  for (name in c("C12_no_model", "C12_not_converged")) {
    spec <- diagram_case_spec(cases[[name]])
    expect_false(sieve_diagram_drawable(spec))
    err <- expect_error(sieve_draw_diagram(spec), class = "sieve_diagram_error")
    expect_identical(err$reason, spec$status$code)
    expect_identical(err$status, spec$status)
    expect_match(conditionMessage(err), spec$status$message, fixed = TRUE)
    expect_null(grDevices::dev.list())
    for (format in c("pdf", "svg", "png")) {
      file <- file.path(dir, paste0(name, ".", format))
      err <- expect_error(sieve_export_diagram(spec, file), class = "sieve_diagram_error")
      expect_identical(err$reason, spec$status$code)
      expect_false(file.exists(file))
      expect_null(grDevices::dev.list())
    }
    # A display change cannot turn the failure into a figure.
    expect_error(sieve_draw_diagram(sieve_diagram_update(spec, covariances = FALSE)),
      class = "sieve_diagram_error"
    )
  }
  expect_identical(diagram_case_spec(cases$C12_no_model)$status$code, "failed")
  expect_identical(diagram_case_spec(cases$C12_not_converged)$status$code, "not_converged")
  # Not a specification at all, or one that was tampered with.
  err <- expect_error(sieve_draw_diagram(list(status = list(code = "ok"))),
    class = "sieve_diagram_error"
  )
  expect_identical(err$reason, "invalid_spec")
  broken <- dr_spec("C01_one_factor")
  broken$edges$from[1] <- "no_such_node"
  err <- expect_error(sieve_export_diagram(broken, file.path(dir, "broken.pdf")),
    class = "sieve_diagram_error"
  )
  expect_identical(err$reason, "invalid_spec")
  expect_false(file.exists(file.path(dir, "broken.pdf")))
  # The improper solution is the one non-clean status that is drawn.
  hey <- sieve_export_diagram(dr_spec("C12_heywood"), file.path(dir, "heywood.pdf"))
  expect_identical(hey$status, "improper")
  expect_true(file.exists(hey$file))
  expect_null(grDevices::dev.list())
  expect_identical(list.files(getwd(), all.files = TRUE), files)
  expect_false(file.exists("Rplots.pdf"))
})

test_that("a failed export leaves no file, no open device, and no changed setting", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C02_two_factors_unit_variance")
  dir <- withr::local_tempdir()
  files <- list.files(getwd(), all.files = TRUE)
  mine <- tempfile(fileext = ".pdf")
  grDevices::pdf(mine)
  dev <- grDevices::dev.cur()
  withr::defer({
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
    unlink(mine)
  })
  graphics::plot.new()
  devices <- grDevices::dev.list()
  par_before <- graphics::par(no.readonly = TRUE)
  untouched <- function() {
    expect_identical(grDevices::dev.list(), devices)
    expect_identical(grDevices::dev.cur(), dev)
    expect_identical(graphics::par(no.readonly = TRUE), par_before)
    expect_identical(list.files(tempdir(), pattern = "^sieve_diagram_"), character(0))
    expect_identical(list.files(getwd(), all.files = TRUE), files)
  }

  # A folder that does not exist.
  gone <- file.path(dir, "no_such_folder", "figure.pdf")
  err <- expect_error(sieve_export_diagram(spec, gone), class = "sieve_diagram_error")
  expect_identical(err$reason, "file")
  expect_false(file.exists(gone))
  untouched()

  # Two nodes on top of each other. The specification refuses positions
  # that do this, so one node could never silently hide another.
  on_top <- data.frame(
    id = "t2_cubes", x = spec$nodes$x[spec$nodes$id == "Visual"],
    y = spec$nodes$y[spec$nodes$id == "Visual"]
  )
  expect_error(sieve_diagram_update(spec, positions = on_top), "on top of Visual")
  near <- transform(on_top, x = x + 0.05)
  expect_error(sieve_diagram_update(spec, positions = near), "on top of Visual")
  # Should such a specification be made by hand, an edge has no path, and
  # nothing is drawn at all, on the caller's device or in a file.
  stacked <- spec
  stacked$nodes$x[stacked$nodes$id == "t2_cubes"] <- on_top$x
  stacked$nodes$y[stacked$nodes$id == "t2_cubes"] <- on_top$y
  for (format in c("pdf", "png")) {
    file <- file.path(dir, paste0("stacked.", format))
    err <- expect_error(sieve_export_diagram(stacked, file), class = "sieve_diagram_error")
    expect_identical(err$reason, "geometry")
    expect_match(conditionMessage(err), "Visual=~t2_cubes", fixed = TRUE)
    expect_false(file.exists(file))
    untouched()
  }
  err <- expect_error(sieve_draw_diagram(stacked), class = "sieve_diagram_error")
  expect_identical(err$reason, "geometry")
  untouched()

  # A failure in the middle of drawing (a font the device does not have):
  # the device opened for the export is closed and its partial file removed.
  file <- file.path(dir, "font.pdf")
  err <- expect_error(suppressWarnings(sieve_export_diagram(spec, file, family = "no such family")),
    class = "sieve_diagram_error"
  )
  expect_identical(err$reason, "render")
  expect_s3_class(err$parent, "error")
  expect_match(conditionMessage(err), conditionMessage(err$parent), fixed = TRUE)
  expect_false(file.exists(file))
  untouched()

  # A failed export does not destroy the file it was to replace.
  kept <- file.path(dir, "kept.pdf")
  sieve_export_diagram(spec, kept)
  size <- file.size(kept)
  expect_error(suppressWarnings(sieve_export_diagram(spec, kept, family = "no such family")),
    class = "sieve_diagram_error"
  )
  expect_identical(file.size(kept), size)
  untouched()

  # Arguments are checked before any device is opened.
  expect_error(sieve_export_diagram(spec, c("a.pdf", "b.pdf")), class = "sieve_diagram_error")
  expect_error(sieve_export_diagram(spec, file, dpi = -1), class = "sieve_diagram_error")
  expect_error(sieve_export_diagram(spec, file, width = 0), class = "sieve_diagram_error")
  expect_error(sieve_export_diagram(spec, file, format = "jpeg"), "should be one of")
  expect_error(sieve_export_diagram(spec, file.path(dir, "huge.png"), dpi = 5000),
    class = "sieve_diagram_error"
  )
  expect_false(file.exists(file.path(dir, "huge.png")))
  expect_false(file.exists(file))
  untouched()
})

test_that("a folder that cannot be written gives a failure and no file", {
  skip_if_not_installed("lavaan")
  skip_on_os("windows")
  spec <- dr_spec("C01_one_factor")
  locked <- file.path(withr::local_tempdir(), "locked")
  dir.create(locked)
  Sys.chmod(locked, "0555")
  withr::defer(Sys.chmod(locked, "0755"))
  skip_if(file.access(locked, 2) == 0, "the folder is writable for this user")
  devices <- grDevices::dev.list()
  err <- expect_error(sieve_export_diagram(spec, file.path(locked, "figure.pdf")),
    class = "sieve_diagram_error"
  )
  expect_identical(err$reason, "file")
  expect_identical(list.files(locked), character(0))
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(list.files(tempdir(), pattern = "^sieve_diagram_"), character(0))
})

test_that("drawing and exporting use no random numbers and leave no stray file", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C09_residual_covariance")
  failed <- diagram_case_spec(diagram_cases()$C12_not_converged)
  dir <- withr::local_tempdir()
  files <- list.files(getwd(), all.files = TRUE)
  devices <- grDevices::dev.list()
  run <- function() {
    invisible(dr_record(spec))
    for (format in c("pdf", "png", "svg")) {
      tryCatch(sieve_export_diagram(spec, file.path(dir, paste0("figure.", format)), dpi = 72),
        sieve_diagram_error = function(e) NULL
      )
    }
    tryCatch(sieve_export_diagram(failed, file.path(dir, "failed.pdf")),
      sieve_diagram_error = function(e) NULL
    )
    tryCatch(sieve_draw_diagram(failed), sieve_diagram_error = function(e) NULL)
  }
  withr::local_preserve_seed()
  set.seed(113)
  before <- get(".Random.seed", envir = globalenv())
  run()
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  # With no stream in the session, none is created.
  rm(".Random.seed", envir = globalenv())
  run()
  expect_false(exists(".Random.seed", envir = globalenv()))
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(list.files(getwd(), all.files = TRUE), files)
  expect_false(file.exists("Rplots.pdf"))
  expect_false(file.exists(file.path(dir, "failed.pdf")))
})

test_that("the drawing fits whatever device it is given", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C05_three_factors")
  natural <- dr_record(spec,
    show_caption = FALSE, width = spec$size$width,
    height = spec$size$height
  )
  expect_equal(natural$scale, 1)
  # A small device scales the whole figure; the type scales with it.
  small <- dr_record(spec, show_caption = FALSE, width = 6, height = 6)
  expect_equal(small$scale, 6 / spec$size$width)
  expect_equal(small$nodes$pt, natural$nodes$pt)
  expect_equal(small$edges[, c("x0", "y0", "x1", "y1")], natural$edges[, c("x0", "y0", "x1", "y1")])
  # A tall device is limited by its width and a wide one by its height;
  # the canvas stays inside the plot region in both.
  for (size in list(c(6, 20), c(30, 4))) {
    rec <- dr_record(spec, width = size[1], height = size[2])
    expect_lte(rec$scale * diff(rec$canvas$xlim), size[1] + 1e-9)
    expect_lte(rec$scale * diff(rec$canvas$ylim), size[2] + 1e-9)
    expect_true(rec$canvas$ylim[1] > rec$ylim[1] && rec$canvas$ylim[2] <= rec$ylim[2] + 1e-9)
    expect_gt(length(rec$caption), 0L)
  }
  # A figure narrower than a readable caption gets a wider page.
  narrow <- dr_spec("C12_heywood")
  style <- sieve_diagram_style(narrow)
  expect_lt(narrow$size$width, 4)
  expect_identical(
    sieve_diagram_export_size(narrow, style, NULL, NULL)[["width"]],
    sieve_diagram_look()$caption_min
  )
  expect_identical(sieve_diagram_export_size(
    narrow, sieve_diagram_style(narrow, show_caption = FALSE),
    NULL, NULL
  )[["width"]], narrow$size$width)
})

# ---- labels, edges, loops, text and captions on further models -------------

# Models outside the acceptance cases that exercise the label placement.
dr_extra_fit <- function(name) {
  d <- sieve_hs9_fixture()$data
  names(d)[match(sieve_hs9_fixture()$item_cols, names(d))] <- paste0("x", 1:9)
  switch(name,
    crossload = lavaan::cfa("visual =~ x1 + x2 + x3 + x9\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9",
      d,
      std.lv = TRUE
    ),
    reciprocal = lavaan::sem("x1 ~ x2 + x4\n x2 ~ x1 + x7", d),
    mediation = lavaan::sem("visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9\n textual ~ visual\n speed ~ visual + textual",
      d,
      std.lv = TRUE
    ),
    path = lavaan::sem("x1 ~ x2 + x3\n x4 ~ x1 + x2", d),
    same_pair = lavaan::sem("x1 ~ x2 + x3\n x4 ~ x1 + x5\n x1 ~~ x4", d)
  )
}

# How many label boxes are crossed by the straight line of another edge.
dr_on_foreign_line <- function(rec) {
  edges <- rec$edges
  lines <- edges[edges$shape == "line", ]
  segs <- as.matrix(lines[, c("x0", "y0", "x1", "y1")])
  labeled <- which(nzchar(edges$label) & edges$shape != "note")
  box <- dr_boxes(edges)
  hits <- 0L
  for (i in labeled) {
    other <- lines$id != edges$id[i]
    hits <- hits + sum(sieve_diagram_crosses(
      segs[other, , drop = FALSE],
      box[i, ] + c(1, -1, 1, -1) * 1e-6
    ))
  }
  hits
}

test_that("no label lies on the line of another edge", {
  skip_if_not_installed("lavaan")
  cases <- diagram_cases()
  for (case in cases[vapply(cases, function(x) x$drawable, NA)]) {
    rec <- do.call(dr_record, c(list(diagram_case_spec(case)), case$draw))
    expect_identical(dr_on_foreign_line(rec), 0L, info = case$name)
  }
  for (name in c("crossload", "reciprocal", "mediation", "path", "same_pair")) {
    spec <- sieve_diagram_spec(dr_extra_fit(name))
    rec <- dr_record(spec)
    expect_identical(dr_on_foreign_line(rec), 0L, info = name)
    # Still the visible part of the specification, each edge once.
    expect_identical(rec$edges$id, spec$edges$id[spec$edges$visible], info = name)
    # No label on a node, and none on another label.
    labeled <- rec$edges[nzchar(rec$edges$label), ]
    box <- dr_boxes(labeled)
    for (i in seq_len(nrow(labeled))) {
      gx <- rep(c(box[i, 1], labeled$label_x[i], box[i, 2]), 3)
      gy <- rep(c(box[i, 3], labeled$label_y[i], box[i, 4]), each = 3)
      for (j in seq_len(nrow(rec$nodes))) {
        expect_false(any(sieve_diagram_inside(rec$nodes[j, ], gx, gy)),
          info = paste(name, labeled$id[i], rec$nodes$id[j])
        )
      }
    }
    clash <- 0L
    for (i in seq_len(nrow(labeled) - 1L)) {
      j <- (i + 1L):nrow(labeled)
      clash <- clash + sum(box[i, 1] < box[j, 2] - 1e-6 & box[j, 1] < box[i, 2] - 1e-6 &
        box[i, 3] < box[j, 4] - 1e-6 & box[j, 3] < box[i, 4] - 1e-6)
    }
    expect_identical(clash, 0L, info = name)
  }
  # The cross-loading runs through the fan of another factor: the label it
  # would have run over was moved along its own line, and stays beside it.
  cross <- dr_record(sieve_diagram_spec(dr_extra_fit("crossload")))$edges
  moved <- cross[cross$id == "textual=~x4", ]
  rest <- cross[cross$id %in% c("textual=~x5", "textual=~x6"), ]
  expect_equal(rest$label_y[1], rest$label_y[2])
  expect_gt(abs(moved$label_y - rest$label_y[1]), 0.05)
  expect_false(moved$on_line)
})

test_that("the values of regressions that cross are written on their own lines", {
  skip_if_not_installed("lavaan")
  for (name in c("C08_structural_left_right", "C08_structural_top_down")) {
    rec <- dr_record(dr_spec(name))$edges
    reg <- rec[rec$type == "regression", ]
    expect_identical(nrow(reg), 4L, info = name)
    expect_true(all(reg$on_line), info = name)
    # On its own line: between the ends, at no distance from the line.
    for (r in seq_len(nrow(reg))) {
      own <- matrix(unlist(reg[r, c("x0", "y0", "x1", "y1")]), 1L)
      expect_lt(sieve_diagram_distance(own, c(reg$label_x[r], reg$label_y[r])), 1e-9,
        label = paste(name, reg$id[r])
      )
      others <- as.matrix(reg[-r, c("x0", "y0", "x1", "y1")])
      expect_gt(min(sieve_diagram_distance(others, c(reg$label_x[r], reg$label_y[r]))), 0.15,
        label = paste(name, reg$id[r])
      )
    }
  }
  # Loadings keep their place: beside their lines for a row of indicators.
  rows <- dr_record(dr_spec("C08_structural_left_right"))$edges
  expect_false(any(rows$on_line[rows$type == "loading"]))
  # Regressions that do not cross keep their values beside their lines.
  med <- dr_record(sieve_diagram_spec(dr_extra_fit("mediation")))$edges
  expect_false(any(med$on_line[med$type == "regression"]))
})

test_that("drawn labels and line types agree with lavaan directly", {
  skip_if_not_installed("lavaan")
  check <- function(case, fit, metric) {
    fit <- diagram_case_fit(fit)
    rec <- dr_record(dr_spec(case))$edges
    pt <- lavaan::parTable(fit)
    pe <- lavaan::parameterEstimates(fit)
    ss <- lavaan::standardizedSolution(fit, type = "std.all")
    key <- function(d) paste0(d$lhs, d$op, d$rhs)
    rows <- rec[rec$id %in% key(pt) & rec$shape != "note", ]
    expect_gt(nrow(rows), 10L)
    at <- match(rows$id, key(pt))
    ordered <- lavaan::lavNames(fit, "ov.ord")
    implied <- pt$free[at] == 0 & pt$op[at] == "~~" & pt$lhs[at] == pt$rhs[at] &
      pt$lhs[at] %in% ordered
    line <- ifelse(implied, "derived", ifelse(pt$free[at] == 0, "fixed", "free"))
    expect_identical(rows$line, line, info = case)
    est <- pe$est[match(rows$id, key(pe))]
    std <- ss$est.std[match(rows$id, key(ss))]
    value <- if (identical(metric, "none")) est else std
    label <- formatC(value, format = "f", digits = 2)
    # A parameter fixed by the analyst shows its constant in the metric in
    # which it was fixed; an implied value is in parentheses.
    constant <- line == "fixed" & abs(value - est) <= 1e-12
    label[constant] <- format(est[constant])
    label[implied] <- paste0("(", label[implied], ")")
    expect_identical(rows$label, label, info = case)
  }
  check("C02_two_factors_unit_variance", "two_unit", "std.all")
  check("C03_two_factors_marker_unstandardized", "two_marker", "none")
  check("C03_two_factors_marker_std_all", "two_marker", "std.all")
  check("C10_ordered_wlsmv_delta", "ordered", "std.all")
  check("C10_binary_wlsmv_delta", "binary", "std.all")
  # Marker identification, unstandardized: the two fixed loadings are the
  # only dashed loadings and the only ones that read 1.
  raw <- dr_record(dr_spec("C03_two_factors_marker_unstandardized"))$edges
  pt <- lavaan::parTable(diagram_case_fit("two_marker"))
  fixed_loadings <- paste0(pt$lhs, pt$op, pt$rhs)[pt$op == "=~" & pt$free == 0]
  expect_setequal(raw$id[raw$type == "loading" & raw$line == "fixed"], fixed_loadings)
  expect_setequal(raw$id[raw$type == "loading" & raw$label == "1"], fixed_loadings)
})

test_that("several edges on one pair of nodes are all drawn, each as what it is", {
  skip_if_not_installed("lavaan")
  fit <- dr_extra_fit("same_pair")
  expect_true(lavaan::lavInspect(fit, "converged"))
  # A regression between x1 and x4 and a covariance of their disturbances.
  # With the residual terms shown, each is drawn as what it is: the
  # regression between the two boxes, the covariance between the two
  # disturbance terms, so the two never share a pair of ends.
  spec <- sieve_diagram_spec(fit, residuals = TRUE)
  expect_identical(spec$edges$cov_class[spec$edges$id == "x1~~x4"], "disturbance")
  rec <- dr_record(spec)$edges
  reg <- rec[rec$id == "x4~x1", ]
  cov <- rec[rec$id == "x1~~x4", ]
  expect_identical(c(reg$type, reg$shape, reg$from, reg$to), c("regression", "line", "x1", "x4"))
  expect_identical(reg$heads, 1)
  expect_identical(c(cov$type, cov$shape, cov$from, cov$to), c("covariance", "arc", "e.x1", "e.x4"))
  expect_identical(cov$heads, 2)
  # Two different paths, and two labels apart.
  expect_gt(sqrt((reg$x0 - cov$x0)^2 + (reg$y0 - cov$y0)^2) +
    sqrt((reg$x1 - cov$x1)^2 + (reg$y1 - cov$y1)^2), 0.2)
  expect_gt(sqrt((reg$label_x - cov$label_x)^2 + (reg$label_y - cov$label_y)^2), 0.2)
  # With the residual terms hidden, the disturbance covariance leaves the
  # figure with them rather than being drawn between x1 and x4, where it
  # would read as a correlation of the two variables; the caption counts it.
  bare_spec <- sieve_diagram_update(spec, residuals = FALSE)
  bare <- dr_record(bare_spec)$edges
  pair <- bare[(bare$from == "x1" & bare$to == "x4") | (bare$from == "x4" & bare$to == "x1"), ]
  expect_identical(pair$id, "x4~x1")
  expect_identical(bare_spec$edges$hidden_reason[bare_spec$edges$id == "x1~~x4"], "residuals")
})

test_that("variance loops are sized to their node and stay on one side", {
  skip_if_not_installed("lavaan")
  angle <- function(node, x, y) atan2(y - node$y, x - node$x) * 180 / pi
  turned <- function(a, b) abs(((a - b + 180) %% 360) - 180)
  for (name in c(
    "C02_two_factors_unit_variance", "C06_five_factors_24_indicators_short_labels",
    "C06_five_factors_24_indicators_left_right", "C08_structural_left_right"
  )) {
    spec <- dr_spec(name)
    rec <- dr_record(spec)
    loops <- rec$edges[rec$edges$type == "variance", ]
    on <- rec$nodes[match(loops$from, rec$nodes$id), ]
    feet <- sqrt((loops$x1 - loops$x0)^2 + (loops$y1 - loops$y0)^2)
    latent <- on$shape == "ellipse"
    across <- ifelse(abs(loops$label_x - on$x) > abs(loops$label_y - on$y), on$h, on$w)
    # The loop of a latent variable spans about half of it, as in the
    # classroom figures; all the loops of one kind have one size.
    expect_true(all(feet[latent] > 0.38 * across[latent]), info = name)
    expect_true(all(feet[latent] < 0.75 * across[latent]), info = name)
    expect_lt(diff(range(feet[latent])), 1e-6, label = paste(name, "latent loops"))
    expect_lt(diff(range(feet[!latent])), 1e-6, label = paste(name, "residual loops"))
    # Every latent loop of a figure sits on the same side of its node.
    side <- spec$edges$angle[match(loops$id[latent], spec$edges$id)]
    expect_identical(length(unique(side)), 1L, info = name)
    # An arc that meets a node stays clear of that node's own loop.
    arcs <- rec$edges[rec$edges$type == "covariance", ]
    for (r in seq_len(nrow(arcs))) {
      for (end in 1:2) {
        id <- if (end == 1) arcs$from[r] else arcs$to[r]
        loop <- loops[loops$from == id, ]
        if (!nrow(loop)) next
        node <- rec$nodes[rec$nodes$id == id, ]
        at <- if (end == 1) angle(node, arcs$x0[r], arcs$y0[r]) else angle(node, arcs$x1[r], arcs$y1[r])
        feet_at <- c(angle(node, loop$x0, loop$y0), angle(node, loop$x1, loop$y1))
        mid <- spec$edges$angle[spec$edges$id == loop$id]
        half <- max(turned(feet_at, mid))
        expect_gt(turned(at, mid), half + 5, label = paste(name, arcs$id[r], id))
      }
    }
  }
  # Five factors side by side: the loops are all on top, and the four arcs
  # at an end factor are spread well beyond the width of an arrowhead.
  spec5 <- dr_spec("C06_five_factors_24_indicators_short_labels")
  expect_true(all(spec5$edges$angle[spec5$edges$type == "variance" &
    spec5$edges$from %in% names(sieve_hs_fixture()$theory_map)] == 90))
  rec <- dr_record(spec5)$edges
  arcs <- rec[rec$type == "covariance", ]
  ends <- data.frame(node = c(arcs$from, arcs$to), x = c(arcs$x0, arcs$x1), y = c(arcs$y0, arcs$y1))
  for (node in c("Spatial", "Reasoning")) {
    at <- ends[ends$node == node, ]
    expect_gt(min(stats::dist(at[, c("x", "y")])), 1.8 * sieve_diagram_look()$head$covariance[2],
      label = paste("ends at", node)
    )
  }
  # On its side, the loops are all on the left, away from the indicators.
  side <- dr_spec("C06_five_factors_24_indicators_left_right")
  expect_true(all(side$edges$angle[side$edges$type == "variance" &
    side$edges$from %in% names(sieve_hs_fixture()$theory_map)] == 180))
  # The two disturbances joined by an arc turn their loops aside together.
  sem <- dr_spec("C08_structural_top_down")
  zeta <- sem$edges$angle[match(
    c("Speed~~Speed", "t24_woody_mccall~~t24_woody_mccall"),
    sem$edges$id
  )]
  expect_equal(sort(zeta), c(225, 315))
})

test_that("hiding relationships gives their room back and moves nothing", {
  skip_if_not_installed("lavaan")
  full <- dr_spec("C09_residual_covariance")
  off <- dr_spec("C09_residual_covariance_factor_covariances_hidden")
  expect_identical(off$nodes[, c("id", "x", "y", "w", "h")], full$nodes[, c("id", "x", "y", "w", "h")])
  expect_lt(off$size$height, full$size$height - 0.5)
  factors <- off$nodes[off$nodes$kind == "latent", ]
  loop_top <- max(factors$y + factors$h / 2) + sieve_diagram_geometry()$loop_length
  expect_lt(off$size$ylim[2] - loop_top, 0.75)
  expect_equal(off$size$ylim[1], full$size$ylim[1])
  bare <- sieve_diagram_update(full, residuals = FALSE, residual_covariances = FALSE)
  expect_gt(bare$size$ylim[1], full$size$ylim[1] + 0.5)
  # Far less tall than a stack of arcs that each leave at their own angle.
  five <- dr_spec("C06_five_factors_24_indicators_short_labels")
  expect_lt(five$size$height, 8.5)
  expect_gt(five$size$width, 2.5 * five$size$height)
})

test_that("residual terms are numbered, and color marks constructs only", {
  skip_if_not_installed("lavaan")
  one <- dr_record(dr_spec("C01_one_factor"))$nodes
  expect_identical(one$index[one$shape == "circle"], 1:5)
  expect_true(all(is.na(one$index[one$shape != "circle"])))
  sem <- dr_record(dr_spec("C08_structural_left_right"))$nodes
  for (letter in c("delta", "epsilon", "zeta")) {
    expect_identical(sem$index[sem$label %in% letter], seq_len(sum(sem$label %in% letter)),
      info = letter
    )
  }
  expect_identical(sum(sem$label %in% "zeta"), 2L)
  # A single term of its kind needs no number.
  med <- dr_record(sieve_diagram_spec(lavaan::sem(
    "Visual =~ t1_visual_perception + t2_cubes + t4_lozenges
     Textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning
     Textual ~ Visual", sieve_hs9_fixture()$data,
    std.lv = TRUE
  )))$nodes
  expect_true(is.na(med$index[med$label %in% "zeta"]))
  # A path model has no construct, so no node gets an accent color.
  path <- sieve_diagram_spec(dr_extra_fit("path"))
  style <- sieve_diagram_style(path)
  expect_identical(unique(style$border[path$nodes$kind == "observed"]), style$line)
  expect_true(all(style$fill == "#ffffff"))
  # In a structural model the observed outcome is neutral, and each
  # construct shares its color with its own indicators only.
  sem_spec <- dr_spec("C08_structural_left_right")
  style <- sieve_diagram_style(sem_spec)
  expect_identical(style$border[sem_spec$nodes$id == "t24_woody_mccall"], style$line)
  expect_identical(length(unique(style$border[sem_spec$nodes$kind == "latent"])), 3L)
  expect_true(all(style$fill == "#ffffff"))
})

test_that("a word hyphen is a hyphen in PDF, and a negative number keeps its minus", {
  skip_if_not_installed("lavaan")
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  dev <- grDevices::dev.cur()
  on_pdf <- sieve_diagram_device_text(c("Woody-McCall", "-0.62", "double-headed", "0.42 [a-b]"))
  grDevices::dev.off(dev)
  unlink(file)
  hyphen <- intToUtf8(173L)
  expect_identical(on_pdf, c(
    paste0("Woody", hyphen, "McCall"), "-0.62",
    paste0("double", hyphen, "headed"), paste0("0.42 [a", hyphen, "b]")
  ))
  expect_null(grDevices::dev.list())
  # Without a PDF device the text is left alone.
  expect_identical(sieve_diagram_device_text("Woody-McCall"), "Woody-McCall")
  expect_identical(sieve_diagram_device_text(quote(delta[1])), quote(delta[1]))
  skip_if_not_installed("pdftools")
  dir <- withr::local_tempdir()
  sem <- sieve_export_diagram(dr_spec("C08_structural_left_right"), file.path(dir, "sem.pdf"))
  text <- gsub("[[:space:]]+", " ", paste(pdftools::pdf_text(sem$file), collapse = " "))
  minus <- intToUtf8(8722L)
  expect_match(text, "Woody-McCall", fixed = TRUE)
  expect_match(text, "digit-symbol", fixed = TRUE)
  expect_no_match(text, paste0("Woody", minus, "McCall"), fixed = TRUE)
  neg <- sieve_export_diagram(dr_spec("C11_long_labels_negative_loading"), file.path(dir, "neg.pdf"))
  text <- paste(pdftools::pdf_text(neg$file), collapse = " ")
  expect_match(text, paste0(minus, "0.62"), fixed = TRUE)
  # The fonts are the standard ones every viewer has.
  expect_true(all(pdftools::pdf_fonts(sem$file)$name %in% c("Helvetica", "Symbol", "Helvetica-Bold")))
})

test_that("a caption gives way to the diagram, and the export reports where a failure arose", {
  skip_if_not_installed("lavaan")
  spec <- dr_spec("C06_five_factors_24_indicators_short_labels")
  caption <- sieve_diagram_figure_caption(spec)
  expect_no_match(sieve_diagram_figure_caption(spec, key = FALSE), "Key:", fixed = TRUE)
  expect_match(caption, "Key: V1 = Visual perception", fixed = TRUE)
  # Set under a small figure, the caption type is reduced before the
  # caption takes more than two fifths of the height; it is never cut.
  roomy <- sieve_diagram_caption_block(caption, 20, 8)
  expect_identical(roomy$scale, 1)
  tight <- sieve_diagram_caption_block(caption, 6, 2)
  expect_lt(tight$scale, 1)
  expect_gte(tight$scale, 0.6)
  expect_identical(paste(tight$lines, collapse = " "), caption)
  expect_identical(paste(roomy$lines, collapse = " "), caption)
  # The measure grows with the figure, more slowly than the figure does.
  expect_identical(sieve_diagram_caption_measure(8), 8 - 2 * sieve_diagram_geometry()$margin)
  expect_identical(sieve_diagram_caption_measure(15), 10)
  expect_equal(sieve_diagram_caption_measure(30), 18)
  # The notation sentence names only what is particular to the figure.
  plain <- sieve_diagram_notation(dr_spec("C02_two_factors_unit_variance"))
  expect_no_match(plain, "ellipse", fixed = TRUE)
  expect_no_match(plain, "curved double-headed arrow", fixed = TRUE)
  expect_match(plain, "dashed line, parameter fixed in estimation", fixed = TRUE)
  # Provenance of an export names the model as well as the options.
  dir <- withr::local_tempdir()
  out <- sieve_export_diagram(
    dr_spec("C03_two_factors_marker_unstandardized"),
    file.path(dir, "marker.pdf")
  )
  expect_identical(out$model_kind, "cfa")
  expect_identical(out$identification, "marker")
  expect_identical(out$nobs, 301L)
  expect_false(is.null(out$model_source))
  expect_identical(names(out$versions), c("SIEVER", "lavaan", "R"))
})

# ---- straight edges clear of the nodes -------------------------------------

# The interior of a drawn path (its points away from the two ends).
dr_interior <- function(p) {
  if (nrow(p) == 2L) {
    t <- seq(0, 1, length.out = 401)
    p <- cbind(p[1, 1] + t * (p[2, 1] - p[1, 1]), p[1, 2] + t * (p[2, 2] - p[1, 2]))
  }
  p[-c(1L, nrow(p)), , drop = FALSE]
}

# Names of the drawn nodes a path runs through that are not its ends.
dr_through <- function(rec, paths, r) {
  p <- paths$path[[r]]
  if (is.null(p)) return(character(0))
  inner <- dr_interior(p)
  ends <- unlist(rec$edges[r, c("from", "to")], use.names = FALSE)
  hit <- character(0)
  for (j in which(!rec$nodes$id %in% ends)) {
    if (any(sieve_diagram_inside(rec$nodes[j, ], inner[, 1], inner[, 2]))) {
      hit <- c(hit, rec$nodes$id[j])
    }
  }
  hit
}

test_that("a straight edge is never drawn through a node that is not one of its ends", {
  skip_if_not_installed("lavaan")
  fit <- dr_extra_fit("mediation")
  # The layout keeps a mediator off the direct line. A position override
  # that puts predictor, mediator, and outcome on one line must still not
  # be drawn as a chain: the direct edge is re-routed as a curve that
  # clears the mediator, and keeps its own value.
  on_line <- data.frame(id = c("visual", "textual", "speed"), x = c(0, 4.5, 9), y = 0)
  spec <- sieve_diagram_spec(fit, layout = "left_right", positions = on_line)
  shown <- spec$edges$id[spec$edges$visible]
  paths <- sieve_diagram_paths(spec)
  r <- match("speed~visual", shown)
  p <- paths$path[[r]]
  textual <- spec$nodes[spec$nodes$id == "textual", ]
  inner <- dr_interior(p)
  expect_false(any(sieve_diagram_inside(textual, inner[, 1], inner[, 2])))
  expect_true(isTRUE(paths$curved[r]))
  expect_gt(nrow(p), 2L)
  expect_identical(sum(paths$curved), 1L)
  # The curve still starts and ends on the outlines of its own two nodes.
  visual <- spec$nodes[spec$nodes$id == "visual", ]
  speed <- spec$nodes[spec$nodes$id == "speed", ]
  expect_true(dr_on_outline(visual, p[1, 1], p[1, 2]))
  expect_true(dr_on_outline(speed, p[nrow(p), 1], p[nrow(p), 2]))
  # Drawn: no path of the figure runs through a foreign node, the direct
  # effect is recorded as curved, and its value sits on or beside its own
  # curve, well clear of the chord through the mediator.
  rec <- dr_record(spec)
  for (k in seq_len(nrow(rec$edges))) {
    expect_identical(dr_through(rec, paths, k), character(0), info = rec$edges$id[k])
  }
  e <- rec$edges[rec$edges$id == "speed~visual", ]
  expect_true(e$curved)
  expect_false(any(rec$edges$curved[rec$edges$id != "speed~visual"]))
  own <- cbind(p[-nrow(p), ], p[-1, ])
  expect_lt(min(sieve_diagram_distance(own, c(e$label_x, e$label_y))), 0.25)
  expect_gt(abs(e$label_y), textual$h / 2 + 0.1)
  expect_identical(e$label, "0.43")
  # The two arrows that arrive at the outcome come from different directions.
  arrive <- rec$edges[rec$edges$to == "speed" & rec$edges$type == "regression", ]
  tips <- rec$nodes[rec$nodes$id == "speed", ]
  angle <- atan2(arrive$y1 - tips$y, arrive$x1 - tips$x)
  expect_gt(abs(diff(angle)), 0.3)
  head_w <- sieve_diagram_look()$head$regression[2]
  expect_gt(sqrt(diff(arrive$x1)^2 + diff(arrive$y1)^2), 1.2 * head_w)
  # A chain of four on one line: every skipping edge is re-routed, and the
  # figure is drawn.
  chain <- lavaan::sem("x4 ~ x1\n x7 ~ x1 + x4\n x9 ~ x1 + x4 + x7", {
    d <- sieve_hs9_fixture()$data
    names(d)[match(sieve_hs9_fixture()$item_cols, names(d))] <- paste0("x", 1:9)
    d
  })
  row <- data.frame(id = c("x1", "x4", "x7", "x9"), x = c(0, 2, 4, 6), y = 0)
  chained <- sieve_diagram_spec(chain,
    layout = "left_right", positions = row, residuals = FALSE,
    variances = FALSE
  )
  cp <- sieve_diagram_paths(chained)
  ids <- chained$edges$id[chained$edges$visible]
  expect_setequal(ids[cp$curved], c("x7~x1", "x9~x1", "x9~x4"))
  crec <- dr_record(chained)
  for (k in seq_len(nrow(crec$edges))) {
    expect_identical(dr_through(crec, cp, k), character(0), info = crec$edges$id[k])
  }
  # Without an override the layout itself keeps the lines clear, so nothing
  # needs re-routing in the acceptance cases.
  for (name in c("C08_mediation_left_right", "C08_mediation_top_down", "C08_structural_left_right")) {
    expect_false(any(sieve_diagram_paths(dr_spec(name))$curved), info = name)
  }
})

# A geometric check of the drawn figure. Everything is measured from the
# drawn record and the dense paths of sieve_diagram_paths(), so nothing
# trusts the renderer's own placement cost. A problem is reported as a
# sentence naming the label or line, so a failure says what is wrong.
dr_dense_segments <- function(spec) {
  paths <- sieve_diagram_paths(spec)$path
  ids <- spec$edges$id[spec$edges$visible]
  do.call(rbind, lapply(seq_along(paths), function(r) {
    p <- paths[[r]]
    if (is.null(p)) {
      return(NULL)
    }
    data.frame(
      x0 = p[-nrow(p), 1], y0 = p[-nrow(p), 2], x1 = p[-1, 1], y1 = p[-1, 2],
      edge = ids[r], stringsAsFactors = FALSE
    )
  }))
}

# Distance from a point to each segment of a table of segments.
dr_seg_dist <- function(segs, at) {
  dx <- segs$x1 - segs$x0
  dy <- segs$y1 - segs$y0
  t <- ((at[1] - segs$x0) * dx + (at[2] - segs$y0) * dy) / pmax(dx^2 + dy^2, 1e-12)
  t <- pmin(1, pmax(0, t))
  sqrt((segs$x0 + t * dx - at[1])^2 + (segs$y0 + t * dy - at[2])^2)
}

# The problems the geometric check finds in one drawn figure: a value over
# the line of another edge (its box grown by `clear`), a value beside its
# line that is nearer to another line than to its own, a value over another
# value, a value beyond the canvas, a variance loop into a node that is not
# its own, an arc too short to carry its value on the line, and (with
# `arcs`) two arcs within 0.04 in of each other for more than 0.4 in.
dr_geometry_problems <- function(spec, rec, arcs = TRUE, clear = 0.012) {
  segs <- dr_dense_segments(spec)
  e <- rec$edges
  labeled <- which(nzchar(e$label) & e$shape != "note")
  box <- dr_boxes(e)
  found <- character(0)
  for (i in labeled) {
    other <- segs$edge != e$id[i]
    grown <- box[i, ] + c(-1, 1, -1, 1) * clear
    hit <- unique(segs$edge[other][sieve_diagram_crosses(as.matrix(segs[other, 1:4]), grown)])
    if (length(hit)) {
      found <- c(found, sprintf(
        "label %s (%s) covers the line of %s", e$label[i], e$id[i],
        paste(hit, collapse = ", ")
      ))
    }
    if (!isTRUE(e$on_line[i])) {
      at <- c(e$label_x[i], e$label_y[i])
      own <- min(dr_seg_dist(segs[!other, ], at))
      far <- dr_seg_dist(segs[other, ], at)
      j <- which.min(far)
      if (length(j) && far[j] < own) {
        found <- c(found, sprintf(
          "label %s (%s) is %.3f in from the line of %s and %.3f in from its own",
          e$label[i], e$id[i], far[j], segs$edge[other][j], own
        ))
      }
    }
    if (box[i, 1] < rec$canvas$xlim[1] - 1e-9 || box[i, 2] > rec$canvas$xlim[2] + 1e-9 ||
      box[i, 3] < rec$canvas$ylim[1] - 1e-9 || box[i, 4] > rec$canvas$ylim[2] + 1e-9) {
      found <- c(found, sprintf("label %s (%s) extends beyond the canvas", e$label[i], e$id[i]))
    }
  }
  for (a in seq_along(labeled)) {
    for (b in seq_along(labeled)) {
      if (a < b) {
        p <- box[labeled[a], ]
        q <- box[labeled[b], ]
        if (p[1] < q[2] - 1e-6 && q[1] < p[2] - 1e-6 && p[3] < q[4] - 1e-6 && q[3] < p[4] - 1e-6) {
          found <- c(found, sprintf(
            "label %s (%s) overlaps label %s (%s)", e$label[labeled[a]],
            e$id[labeled[a]], e$label[labeled[b]], e$id[labeled[b]]
          ))
        }
      }
    }
  }
  for (id in e$id[e$shape == "loop"]) {
    s <- segs[segs$edge == id, ]
    host <- e$from[e$id == id]
    for (k in which(rec$nodes$id != host)) {
      n <- rec$nodes[k, ]
      n$w <- n$w + 2 * clear
      n$h <- n$h + 2 * clear
      if (any(sieve_diagram_inside(n, c(s$x0, s$x1), c(s$y0, s$y1)))) {
        found <- c(found, sprintf("the loop of %s runs into node %s", host, rec$nodes$id[k]))
      }
    }
  }
  heads <- 2 * sieve_diagram_look()$head$covariance[1]
  for (i in labeled) {
    if (e$shape[i] == "arc" && isTRUE(e$on_line[i])) {
      s <- segs[segs$edge == e$id[i], ]
      len <- sum(sqrt((s$x1 - s$x0)^2 + (s$y1 - s$y0)^2))
      if (len - heads - e$label_w[i] < 0.1) {
        found <- c(found, sprintf(
          "arc %s is %.2f in long, too short for its value on the line",
          e$id[i], len
        ))
      }
    }
  }
  if (arcs) {
    arc_ids <- e$id[e$shape == "arc"]
    for (a in seq_along(arc_ids)) {
      for (b in seq_along(arc_ids)) {
        if (a < b) {
          p <- segs[segs$edge == arc_ids[a], ]
          q <- segs[segs$edge == arc_ids[b], ]
          near <- vapply(
            seq_len(nrow(p)), function(r) min(dr_seg_dist(q, c(p$x0[r], p$y0[r]))),
            numeric(1)
          )
          run <- sum(sqrt((p$x1 - p$x0)^2 + (p$y1 - p$y0)^2)[near < 0.04])
          if (run > 0.4) {
            found <- c(found, sprintf(
              "arcs %s and %s run within 0.04 in of each other for %.2f in",
              arc_ids[a], arc_ids[b], run
            ))
          }
        }
      }
    }
  }
  found
}

test_that("every value is placed clear of every other line, value, and node, and the canvas holds it", {
  skip_if_not_installed("lavaan")
  hs <- sieve_hs_fixture()$labels
  hs9 <- sieve_hs9_fixture()
  # Five correlated factors with short labels: no covariance value cuts
  # the arc of another pair.
  spec <- dr_spec("C06_five_factors_24_indicators_short_labels")
  rec <- dr_record(spec)
  expect_identical(dr_geometry_problems(spec, rec), character(0))
  # A residual covariance in the left-right layout with short labels: the
  # arc, its value, and the turned loops of the two joined terms have room,
  # and the arc is long enough to carry its value or the value is beside it.
  c09x <- sieve_diagram_spec(diagram_case_fit("rescov"),
    labels = hs, layout = "left_right",
    label_mode = "short"
  )
  expect_identical(dr_geometry_problems(c09x, dr_record(c09x)), character(0))
  # One factor with ten indicators and short labels: every loading value is
  # clearly nearer to its own line than to the neighboring one.
  fan <- lavaan::cfa(paste("G =~", paste(sieve_hs_fixture()$item_cols[1:10], collapse = " + ")),
    sieve_hs_fixture()$data,
    std.lv = TRUE
  )
  t9 <- sieve_diagram_spec(fan, labels = hs, label_mode = "short")
  expect_identical(dr_geometry_problems(t9, dr_record(t9)), character(0))
  # Wide unstandardized values: the canvas is sized from the width of the
  # values as written, so a value with five decimals is never cut off at
  # the edge, and the figure grows with the values.
  d10 <- hs9$data
  d10[hs9$item_cols] <- d10[hs9$item_cols] * 10
  wide <- lavaan::cfa(diagram_case_syntax("three"), d10, std.lv = TRUE)
  k2 <- sieve_diagram_spec(wide,
    labels = hs, standardized = "none", label_mode = "short",
    layout = "left_right", digits = 5L
  )
  expect_identical(dr_geometry_problems(k2, dr_record(k2)), character(0))
  k0 <- sieve_diagram_spec(wide,
    labels = hs, standardized = "none", label_mode = "short",
    layout = "left_right", digits = 1L
  )
  expect_gt(k2$size$width, k0$size$width)
  # An equality constraint with a long label: the figure carries a short
  # key after the value, the caption explains the key, and nothing is
  # written over a neighboring line.
  eq <- lavaan::cfa(gsub(
    "Visual =~ t1_visual_perception \\+ t2_cubes \\+ t4_lozenges",
    paste(
      "Visual =~ lambda_visual*t1_visual_perception +",
      "lambda_visual*t2_cubes + lambda_visual*t4_lozenges"
    ),
    diagram_case_syntax("three")
  ), hs9$data, std.lv = TRUE)
  t4 <- sieve_diagram_spec(eq, labels = hs, label_mode = "short")
  rec4 <- dr_record(t4)
  expect_identical(dr_geometry_problems(t4, rec4), character(0))
  tied <- rec4$edges[rec4$edges$type == "loading" & rec4$edges$from == "Visual", ]
  expect_identical(nrow(tied), 3L)
  expect_true(all(grepl(" [c1]", tied$label, fixed = TRUE)))
  expect_match(sieve_diagram_figure_caption(t4),
    "Constraint labels in the figure: c1 = lambda_visual.",
    fixed = TRUE
  )
  # Six correlated factors with short labels: every value is clear. (The
  # arcs at the end factors run close together, so the arc-pair check is
  # left out of this case.)
  six <- sieve_with_seed(20261012, {
    n <- 500
    k <- 6
    f <- matrix(stats::rnorm(n * k), n, k) %*% chol(matrix(0.45, k, k) + diag(0.55, k))
    items <- lapply(seq_len(k), function(j) {
      sapply(1:6, function(i) 0.7 * f[, j] + stats::rnorm(n, sd = 0.7))
    })
    d <- as.data.frame(do.call(cbind, items))
    names(d) <- paste0("i", seq_len(36))
    d
  })$value
  six_names <- c("Autonomy", "Competence", "Relatedness", "Engagement", "Burnout", "Turnover")
  six_syntax <- paste(sprintf("%s =~ %s", six_names, vapply(1:6, function(j) {
    paste(paste0("i", (j - 1) * 6 + 1:6), collapse = " + ")
  }, character(1))), collapse = "\n")
  s1 <- sieve_diagram_spec(lavaan::cfa(six_syntax, six, std.lv = TRUE), label_mode = "short")
  expect_identical(dr_geometry_problems(s1, dr_record(s1), arcs = FALSE), character(0))
  # Every value of the acceptance cases was placed at zero cost, so the
  # record and the geometric check agree.
  for (case in diagram_cases()) {
    if (!case$drawable) {
      next
    }
    drawn <- do.call(dr_record, c(list(diagram_case_spec(case)), case$draw))
    expect_true(all(drawn$edges$cost == 0), info = case$name)
  }
})

test_that("a value that cannot be placed clear of everything is reported on export, never passed silently", {
  skip_if_not_installed("lavaan")
  # Three constructs of the mediation model forced close together: there is
  # no clean place for every value, the record carries the cost of each,
  # and the export warns with a classed condition that names the labels.
  tight <- data.frame(id = c("Visual", "Textual", "Speed"), x = c(0, 1.2, 2.4), y = c(0, 0.9, 0))
  spec <- sieve_diagram_spec(diagram_case_fit("mediation"),
    layout = "left_right",
    positions = tight
  )
  rec <- dr_record(spec)
  crowded <- rec$edges$id[rec$edges$cost > 0]
  expect_gt(length(crowded), 0L)
  dir <- withr::local_tempdir()
  out <- NULL
  expect_warning(out <- sieve_export_diagram(spec, file.path(dir, "tight.pdf")),
    class = "sieve_diagram_warning"
  )
  expect_true(file.exists(out$file))
  cnd <- tryCatch(sieve_export_diagram(spec, file.path(dir, "tight2.pdf")),
    sieve_diagram_warning = function(w) w
  )
  expect_identical(cnd$reason, "placement")
  for (id in crowded) {
    expect_match(conditionMessage(cnd), id, fixed = TRUE)
  }
  # A figure whose every value is clear exports without a warning.
  expect_no_warning(sieve_export_diagram(dr_spec("C05_three_factors"), file.path(dir, "clear.pdf")))
})

test_that("a residual term's variance loop is round, with its two ends apart", {
  # A loop cut from an ellipse whose length beyond the term equaled its
  # width, with the part sunk into the term added to its length, would be
  # taller than wide and read as a narrow oval whose arrowheads nearly
  # meet. The ellipse's two diameters are equal.
  g <- sieve_diagram_geometry()
  expect_equal(g$residual_loop_length + g$residual_loop_sink, g$residual_loop_width)
  expect_gte(g$residual_loop_width, g$loop_width)
  spec <- dr_spec("C05_three_factors")
  paths <- sieve_diagram_paths(spec)
  edges <- spec$edges[spec$edges$visible, , drop = FALSE]
  kind <- spec$nodes$kind[match(edges$from, spec$nodes$id)]
  loops <- which(edges$type == "variance" & kind == "residual")
  expect_gt(length(loops), 0L)
  for (r in loops) {
    path <- paths$path[[r]]
    rad <- edges$angle[r] * pi / 180
    along <- path %*% c(cos(rad), sin(rad))
    across <- path %*% c(-sin(rad), cos(rad))
    # Wider than it reaches out from the term, as a round loop cut by the
    # term's circle is, never taller than wide.
    expect_gt(diff(range(across)), diff(range(along)))
    # The two arrowheads sit well apart on the term (more than 0.25 in).
    chord <- sqrt(sum((path[1L, ] - path[nrow(path), ])^2))
    expect_gt(chord, 0.25)
  }
})

test_that("the ends of residual covariance arcs stay clear of a round residual loop's feet", {
  skip_if_not_installed("lavaan")
  # A chain of residual covariances: the middle term has an arc on each
  # side and its loop between them. Arcs kept clear of the loop's estimated
  # footprint, atan2(width / 2, reach), alone would not be enough, since the
  # round loop's feet exceed it and an arc's arrowhead would meet the loop's
  # (5 degrees apart).
  hs <- sieve_hs_data()
  items <- grep("^t[0-9]+_", names(hs), value = TRUE)[1:12]
  short <- sub("_.*$", "", items)
  d <- hs[, items]
  names(d) <- short
  fit <- lavaan::cfa(
    paste("g =~", paste(short, collapse = " + "), "\nt3 ~~ t4\nt4 ~~ t5"),
    d,
    std.lv = TRUE
  )
  for (layout in c("top_down", "left_right")) {
    spec <- sieve_diagram_update(sieve_diagram_spec(fit, layout = layout), label_mode = "short")
    paths <- sieve_diagram_paths(spec)
    edges <- spec$edges[spec$edges$visible, , drop = FALSE]
    nodes <- spec$nodes
    gaps <- numeric(0)
    for (r in which(edges$type == "variance" & nodes$kind[match(edges$from, nodes$id)] == "residual")) {
      i <- match(edges$from[r], nodes$id)
      at <- c(nodes$x[i], nodes$y[i])
      angle <- function(p) atan2(p[2L] - at[2L], p[1L] - at[1L]) * 180 / pi
      loop <- paths$path[[r]]
      # The measured footprint is the loop's own: its feet lie within it.
      foot <- sieve_diagram_loop_foot(nodes, i, edges$angle[r])
      expect_gte(foot + 1e-6, max(abs(((c(angle(loop[1L, ]), angle(loop[nrow(loop), ])) -
        edges$angle[r]) + 180) %% 360 - 180)))
      feet <- c(angle(loop[1L, ]), angle(loop[nrow(loop), ]))
      for (a in which(edges$type == "covariance" & (edges$from == edges$from[r] | edges$to == edges$from[r]))) {
        arc <- paths$path[[a]]
        ends <- list(arc[1L, ], arc[nrow(arc), ])
        near <- ends[[which.min(vapply(ends, function(p) sum((p - at)^2), numeric(1)))]]
        gaps <- c(gaps, min(abs(((angle(near) - feet) + 180) %% 360 - 180)))
      }
    }
    expect_gt(length(gaps), 0L)
    # At least the arrowheads' half widths and some air, 0.08 in along the
    # outline: about 21 degrees on a residual term.
    expect_gt(min(gaps), 15)
  }
})

test_that("a residual term with several covariance arcs keeps the angular clearance, so their values are placed", {
  skip_if_not_installed("lavaan")
  # Four arcs at one term: the wider gap for two arcs at most would push
  # them beside the neighbors' loops, where their values do not fit.
  fit <- lavaan::cfa(
    "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9\nx5 ~~ x4\nx5 ~~ x6\nx5 ~~ x2\nx5 ~~ x8",
    lavaan::HolzingerSwineford1939,
    std.lv = TRUE
  )
  dir <- withr::local_tempdir()
  expect_no_warning(sieve_export_diagram(sieve_diagram_spec(fit), file.path(dir, "star.pdf")))
})
