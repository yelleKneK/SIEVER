# fct_project.R
# The project as a whole: what each column of the data is for (the variable
# roles), and the project snapshot that saves a session and opens it again.
#
# Variable roles. A column's role (item, identifier, group, criterion, antecedent,
# auxiliary) is independent of how the column is stored. On load a role is
# proposed for every column (sieve_propose_roles() in utils_detect.R) and
# shown for review; the researcher can change any of them. A role change
# that moves a column into or out of the item pool changes the item data,
# the item ledger, the roles table and the list of non item columns
# together or not at all.
#
# Project snapshot. sieve_project_snapshot() copies the session into one
# plain list (no reactive object, no environment, no fitted model), which
# sieve_project_write() saves and sieve_project_read() reads back, checking
# the schema version and the contents. sieve_project_restore() puts a
# checked snapshot into the session in one step and gives the data a new
# dataset identity, so nothing computed before the restore is applied to it.
#
# The first half of the file is plain R with no Shiny in it; the functions
# that touch the shared state say so.

# ---- Variable roles ---------------------------------------------------------

#' Stop with a variable role error
#'
#' @param ... Passed to [sprintf()].
#' @return Does not return.
#' @author Ken Kelley
#' @noRd
sieve_role_stop <- function(...) {
  stop(structure(
    list(message = sprintf(...), call = NULL),
    class = c("sieve_role_error", "error", "condition")
  ))
}

#' The roles table that describes a loaded data frame
#'
#' Returns `roles` when it describes `raw` and `items` (one row per column
#' of `raw`, in order, roles from the vocabulary, the item rows exactly the
#' columns of `items`). Otherwise a table is derived: every column of
#' `items` is an item, and every other column gets the role
#' `sieve_propose_roles()` proposes for it, or auxiliary when that would be
#' item. A session whose state was assembled without a roles table is read
#' this way, so every consumer sees a complete table.
#'
#' @param raw The data as loaded.
#' @param items The analytic item data.
#' @param roles A roles table, or `NULL`.
#' @return A roles table (see `sieve_propose_roles()`).
#' @author Ken Kelley
#' @noRd
sieve_roles_table <- function(raw, items, roles = NULL) {
  if (is.null(raw)) {
    return(NULL)
  }
  pool <- names(items) %||% character(0)
  usable <- is.data.frame(roles) &&
    all(c("variable", "role") %in% names(roles)) &&
    identical(as.character(roles$variable), names(raw)) &&
    all(roles$role %in% sieve_role_levels()) &&
    setequal(roles$variable[roles$role == "item"], pool)
  described <- c("proposed", "reason", "numeric", "n_distinct", "n_missing")
  if (usable && all(described %in% names(roles))) {
    # A roles table without a validity kind column gets the column.
    if (!"kind" %in% names(roles)) {
      roles$kind <- rep(NA_character_, nrow(roles))
    }
    return(roles)
  }
  out <- sieve_propose_roles(raw)
  if (usable) {
    # The roles are good and the columns that describe them are not all
    # there: keep the roles (and the kinds), describe them afresh.
    out$role <- as.character(roles$role)
    out$reason[out$role != out$proposed] <- "the role it was given"
    if ("kind" %in% names(roles)) {
      out$kind <- as.character(roles$kind)
    }
    return(out)
  }
  is_item <- out$variable %in% pool
  demoted <- !is_item & out$role == "item"
  out$role[demoted] <- "auxiliary"
  out$reason[demoted] <- "it is not in the item pool"
  out$role[is_item] <- "item"
  out$reason[is_item & out$proposed != "item"] <- "it is in the item pool"
  out$proposed <- out$role
  out
}

#' The roles of a bundled example
#'
#' A bundled example declares which columns are its items. The other
#' columns get the role their content proposes (identifier, group,
#' auxiliary); nothing is left for review, because the example's item pool
#' is not a guess.
#'
#' @param df The example data.
#' @param item_cols The item columns the example declares.
#' @return A roles table.
#' @author Ken Kelley
#' @noRd
sieve_declared_roles <- function(df, item_cols) {
  out <- sieve_roles_table(df, df[, item_cols, drop = FALSE])
  out$reason[out$role == "item"] <- "the bundled example declares it an item"
  out
}

#' Change variable roles, on copies
#'
#' Applies the requested roles to copies of the item data, the ledger, the
#' roles table and the ledger rows held aside, and returns them. A column
#' that stops being an item leaves the item data and the ledger (the data
#' as loaded keep it), and its ledger row is held aside under its name, so
#' that nothing the researcher set for it (its name, wording, construct,
#' facet, type, included or excluded status, declared bounds, notes) is
#' lost. A column that becomes an item is added to the item data with the
#' values as loaded and gets its held row back when there is one, or a
#' fresh ledger row otherwise. Changes among the other roles touch only the
#' roles table. The function stops, changing nothing, when:
#'
#' * a variable is not in the data, or a role is not one of the five;
#' * a column that is not stored as a number is asked to be an item;
#' * an item whose values are not the values as loaded (it was reverse
#'   scored here), or that is marked as reverse scored, is asked to leave
#'   the item pool: its values would come back as loaded while the row
#'   says otherwise, so it is returned to its values as loaded first;
#' * no item would be left.
#'
#' @param raw The data as loaded.
#' @param items The analytic item data.
#' @param meta The item ledger.
#' @param roles The roles table, or `NULL` to derive one.
#' @param changes A character vector of roles named by variable.
#' @param held The ledger rows held aside (a list of one row data frames
#'   named by variable), as the state keeps them in `held_rows`.
#' @return A list with `items`, `meta`, `roles`, `id_vars` (the non item
#'   columns, in data order), `held` (the rows held aside after the
#'   change), `changed` (a data frame with `variable`, `from`, `to` for the
#'   roles that changed), and `pool_changed` (whether the item pool
#'   changed).
#' @author Ken Kelley
#' @noRd
sieve_set_roles <- function(raw, items, meta, roles = NULL, changes, held = list()) {
  if (is.null(raw) || is.null(items) || is.null(meta)) {
    sieve_role_stop("No data are loaded, so there are no roles to change.")
  }
  if (NROW(raw) != NROW(items)) {
    sieve_role_stop("The item data and the data as loaded have different numbers of rows, so no role was changed.")
  }
  roles <- sieve_roles_table(raw, items, roles)
  vars <- names(changes)
  if (is.null(vars) || anyNA(vars) || any(!nzchar(vars)) || anyDuplicated(vars)) {
    sieve_role_stop("Each role change has to name one variable, once.")
  }
  unknown <- setdiff(vars, roles$variable)
  if (length(unknown)) {
    sieve_role_stop(
      "%s %s not in the data, so no role was changed.",
      paste(encodeString(unknown, quote = "\""), collapse = ", "),
      if (length(unknown) == 1L) "is" else "are"
    )
  }
  bad <- setdiff(unname(as.character(changes)), sieve_role_levels())
  if (length(bad)) {
    sieve_role_stop(
      "\"%s\" is not a variable role. The roles are: %s.",
      bad[1], paste(sieve_role_levels(), collapse = ", ")
    )
  }
  meta <- sieve_meta_keying(meta)
  held <- if (is.list(held) && !is.data.frame(held)) held else list()
  changed <- data.frame(
    variable = character(0), from = character(0), to = character(0),
    stringsAsFactors = FALSE
  )
  position <- function(v) match(v, names(raw))
  for (v in vars) {
    i <- match(v, roles$variable)
    from <- roles$role[i]
    to <- changes[[v]]
    if (identical(from, to)) {
      next
    }
    if (identical(to, "item")) {
      if (!is.numeric(raw[[v]])) {
        sieve_role_stop(
          "%s is not stored as a number (it is stored as %s), so it cannot be an item. No role was changed.",
          v, class(raw[[v]])[1]
        )
      }
      kept <- held[[v]]
      # A held row and the ledger can differ in the column that records
      # where each type came from (one holds it, the other does not); the
      # held row takes the ledger's form, so the check below can match it.
      if (is.data.frame(kept) && "type_source" %in% names(meta) && !"type_source" %in% names(kept)) {
        kept$type_source <- sieve_type_source(kept)
      } else if (is.data.frame(kept) && !"type_source" %in% names(meta)) {
        kept$type_source <- NULL
      }
      if (is.data.frame(kept) && nrow(kept) == 1L && identical(as.character(kept$item), v) &&
        setequal(names(kept), names(meta))) {
        # The row the column had when it left the pool comes back with it,
        # its row name included.
        row <- kept[, names(meta), drop = FALSE]
        held[[v]] <- NULL
        if (!length(held)) {
          held <- list()
        }
      } else {
        label <- attr(raw[[v]], "label")
        label <- if (is.character(label) && length(label) == 1L && !is.na(label) && nzchar(label)) label else v
        row <- sieve_build_meta(raw, v, labels = stats::setNames(label, v))
        for (extra in setdiff(names(meta), names(row))) {
          row[[extra]] <- meta[[extra]][NA_integer_]
        }
        row <- row[, names(meta), drop = FALSE]
      }
      # The new item takes the place its column has in the data, so a role
      # that is changed and changed back leaves the pool in its first order.
      after_it <- which(position(meta$item) > position(v))
      at <- if (length(after_it)) min(after_it) else nrow(meta) + 1L
      meta <- rbind(
        meta[seq_len(at - 1L), , drop = FALSE], row,
        meta[seq_len(nrow(meta) - at + 1L) + at - 1L, , drop = FALSE]
      )
      keep <- names(items)
      after_col <- which(position(keep) > position(v))
      at_col <- if (length(after_col)) min(after_col) else length(keep) + 1L
      items[[v]] <- raw[[v]]
      items <- items[, append(keep, v, after = at_col - 1L), drop = FALSE]
    } else if (identical(from, "item")) {
      idx <- which(meta$item == v)
      if (!identical(as.numeric(items[[v]]), as.numeric(raw[[v]]))) {
        sieve_role_stop(
          paste(
            "The values of %s are not its values as loaded (it was reverse scored here), so it",
            "stays an item: what was done to it is kept in the item ledger and would be lost with",
            "its row. Return it to its values as loaded first (the Reverse? cell). No role was changed."
          ),
          v
        )
      }
      if (any(meta$reverse[idx] %in% TRUE)) {
        sieve_role_stop(
          paste(
            "%s is recorded as already reverse scored in the data as loaded, so it stays an item:",
            "that record is kept in the item ledger and would be lost with its row. No role was changed."
          ),
          v
        )
      }
      held[[v]] <- meta[idx, , drop = FALSE]
      meta <- meta[-idx, , drop = FALSE]
      items <- items[, setdiff(names(items), v), drop = FALSE]
    }
    roles$role[i] <- to
    # A validity kind belongs to a criterion; it goes with the role.
    if (!identical(to, "criterion") && "kind" %in% names(roles)) {
      roles$kind[i] <- NA_character_
    }
    changed[nrow(changed) + 1L, ] <- list(v, from, to)
  }
  if (!any(roles$role == "item")) {
    sieve_role_stop("At least one variable has to be an item. No role was changed.")
  }
  list(
    items = items,
    meta = meta,
    roles = roles,
    id_vars = roles$variable[roles$role != "item"],
    held = held,
    changed = changed,
    pool_changed = any(changed$from == "item" | changed$to == "item")
  )
}

#' One sentence for a role change
#'
#' @param variable,from,to The variable and its roles before and after.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_role_note <- function(variable, from, to) {
  a <- function(role) if (role %in% c(
    "item",
    "identifier",
    "antecedent",
    "auxiliary"
  )) paste(
    "an",
    role
  ) else paste(
    "a",
    role
  )
  what <- if (identical(to, "item")) {
    "it joins the item pool with its values as loaded and the ledger row it had, or a new one"
  } else if (identical(from, "item")) {
    "it leaves the item pool; its ledger row is kept aside and returns with it, and the data as loaded keep it"
  } else {
    "the item pool is unchanged"
  }
  sprintf("Set the role of %s to %s (it was %s): %s", variable, to, a(from), what)
}

#' Change variable roles in the shared state
#'
#' Tries the change with `sieve_set_roles()` and, when it can be made,
#' writes the item data, the ledger, the roles table, the non item columns
#' and the ledger rows held aside to the state together and logs each
#' change. A change to the item
#' pool withdraws the confirmation of the item types, because the estimator
#' decision was made for another pool. Because the item data and the ledger
#' change, the revision changes with them, and every result kept from the
#' earlier pool is out of date by the rule of `sieve_stale_reason()`.
#'
#' @param state The shared state store.
#' @param changes A character vector of roles named by variable.
#' @return Invisibly, a list with `changed` (a data frame of the roles that
#'   changed, possibly with no rows) and `refused` (the reason nothing was
#'   changed, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_apply_roles <- function(state, changes) {
  shiny::isolate({
    out <- tryCatch(
      sieve_set_roles(state$raw, state$items, state$meta, state$roles, changes,
        held = state$held_rows %||% list()
      ),
      sieve_role_error = function(e) e
    )
    if (inherits(out, "sieve_role_error")) {
      return(invisible(list(changed = NULL, refused = conditionMessage(out))))
    }
    if (!nrow(out$changed)) {
      return(invisible(list(changed = out$changed, refused = NULL)))
    }
    sieve_ensure_dataset(state)
    state$items <- out$items
    sieve_write_ledger(state, out$meta)
    state$roles <- out$roles
    state$held_rows <- out$held
    if (isTRUE(out$pool_changed)) {
      state$confirmed <- FALSE
    }
    for (i in seq_len(nrow(out$changed))) {
      ch <- out$changed[i, ]
      sieve_log(
        state,
        sieve_role_note(ch$variable, ch$from, ch$to),
        sprintf("# variable role: %s is %s", encodeString(ch$variable, quote = "\""), ch$to)
      )
    }
    invisible(list(changed = out$changed, refused = NULL))
  })
}

#' Set the validity kind of criteria in the shared state
#'
#' The kind (see `sieve_role_kinds()`) is written into the `kind` column of
#' the roles table for the named variables and logged. A variable whose
#' role is not criterion keeps no kind: the call is refused for it with the
#' reason, and nothing is written for any variable.
#'
#' @param state The shared state store.
#' @param vars The variables (criteria).
#' @param kind One of `sieve_role_kinds()`, or `NA` (or `""`) to clear it.
#' @return Invisibly, a list with `changed` (the variables whose kind
#'   changed) and `refused` (the reason nothing was changed, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_apply_kind <- function(state, vars, kind) {
  shiny::isolate({
    kind <- as.character(kind)
    if (!length(kind) || is.na(kind[1]) || !nzchar(kind[1])) {
      kind <- NA_character_
    } else if (!kind[1] %in% sieve_role_kinds()) {
      return(invisible(list(changed = character(0), refused = sprintf(
        "\"%s\" is not a validity kind. The kinds are: %s.", kind[1],
        paste(sieve_role_kinds(), collapse = ", ")
      ))))
    }
    roles <- sieve_roles_table(state$raw, state$items, state$roles)
    if (is.null(roles)) {
      return(invisible(list(changed = character(0), refused = "No data are loaded.")))
    }
    vars <- unique(as.character(vars))
    at <- match(vars, roles$variable)
    if (anyNA(at)) {
      return(invisible(list(changed = character(0), refused = sprintf(
        "%s %s not in the data, so no kind was set.", sieve_name_some(vars[is.na(at)]),
        if (sum(is.na(at)) == 1L) "is" else "are"
      ))))
    }
    not_criterion <- vars[roles$role[at] != "criterion"]
    if (length(not_criterion)) {
      return(invisible(list(changed = character(0), refused = sprintf(
        paste(
          "A validity kind belongs to a criterion, and %s %s not %s (give %s the role criterion",
          "first). No kind was set."
        ),
        sieve_name_some(not_criterion), if (length(not_criterion) == 1L) "is" else "are",
        if (length(not_criterion) == 1L) "one" else "criteria",
        if (length(not_criterion) == 1L) "it" else "them"
      ))))
    }
    before <- as.character(roles$kind[at])
    same <- (before == kind[1]) %in% TRUE | (is.na(before) & is.na(kind[1]))
    changed <- vars[!same]
    if (!length(changed)) {
      return(invisible(list(changed = character(0), refused = NULL)))
    }
    roles$kind[match(changed, roles$variable)] <- kind[1]
    state$roles <- roles
    sieve_log(
      state,
      sprintf(
        "Validity kind of %s: %s", sieve_name_some(changed),
        if (is.na(kind[1])) "not stated" else kind[1]
      ),
      sprintf(
        "# validity kind: %s is %s", encodeString(changed, quote = "\""),
        if (is.na(kind[1])) "not stated" else kind[1]
      )
    )
    invisible(list(changed = changed, refused = NULL))
  })
}

#' The variables that have a role
#'
#' @param state The shared state store.
#' @param role One or more roles.
#' @return The names of the columns of the loaded data with that role, in
#'   data order.
#' @author Ken Kelley
#' @noRd
sieve_role_vars <- function(state, role) {
  roles <- shiny::isolate(sieve_roles_table(state$raw, state$items, state$roles))
  if (is.null(roles)) {
    return(character(0))
  }
  roles$variable[roles$role %in% role]
}

#' A count of the variables in each role, in words
#'
#' @param roles A roles table.
#' @return A length one character string such as
#'   `"9 items, 1 identifier, 2 groups, 1 criterion"`.
#' @author Ken Kelley
#' @noRd
sieve_roles_summary <- function(roles) {
  plural <- c(
    item = "items", identifier = "identifiers", group = "groups",
    criterion = "criteria", antecedent = "antecedents", auxiliary = "auxiliary variables"
  )
  single <- c(
    item = "item", identifier = "identifier", group = "group",
    criterion = "criterion", antecedent = "antecedent", auxiliary = "auxiliary variable"
  )
  n <- vapply(sieve_role_levels(), function(r) sum(roles$role == r), integer(1))
  n <- n[n > 0L]
  paste(sprintf("%d %s", n, ifelse(n == 1L, single[names(n)], plural[names(n)])), collapse = ", ")
}

# ---- Project snapshot -------------------------------------------------------

#' The project schema version this SIEVER writes
#'
#' The schema version belongs to the layout of the snapshot, not to the
#' package: it changes only when a field is added, removed or changes its
#' meaning, and each change comes with an upgrade step in
#' `sieve_project_steps()`. Schema 2 holds, beyond schema 1, the ledger
#' rows held aside and the content key of the records and settings.
#' Schema 3 also records compact, explicit replay inputs, and its
#' additional fields hold the screening, sampling, scoring, and decision
#' settings (a schema 2 file that lacks one gets its default).
#'
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_project_schema <- function() {
  3L
}

#' Stop with a project error
#'
#' @param ... Passed to [sprintf()].
#' @return Does not return.
#' @author Ken Kelley
#' @noRd
sieve_project_stop <- function(...) {
  stop(structure(
    list(message = sprintf(...), call = NULL),
    class = c("sieve_project_error", "error", "condition")
  ))
}

#' Where an object holds something that is not plain data
#'
#' A snapshot is data: vectors, lists and data frames, with their
#' attributes. Environments, functions, external pointers, unevaluated
#' code and S4 objects (a fitted model is one) do not belong in it: they
#' cannot be relied on to mean the same thing in another session, and code
#' in a file that is opened should never be run.
#'
#' @param x Any object.
#' @param path The name the object is reported under.
#' @return The paths of the elements that are not plain data (possibly
#'   empty).
#' @author Ken Kelley
#' @noRd
sieve_not_plain <- function(x, path = "snapshot") {
  if (is.null(x)) {
    return(character(0))
  }
  if (isS4(x) || !(is.atomic(x) || is.list(x)) || is.function(x) || is.environment(x) ||
    is.language(x)) {
    return(path)
  }
  out <- character(0)
  at <- attributes(x)
  for (name in setdiff(names(at), c("names", "class", "row.names", "levels", "dim", "dimnames"))) {
    a <- at[[name]]
    # An attribute that is a bare vector (a variable label, the value labels
    # of an SPSS column) is plain without a look inside.
    if (sieve_bare_plain(a)) next
    out <- c(out, sieve_not_plain(a, sprintf("attr(%s, \"%s\")", path, name)))
  }
  if (is.list(x)) {
    n <- length(x)
    # Elements that are bare vectors (the columns of a data frame, the
    # stored copies of the replay data) are plain; only the others are
    # walked, since walking every column makes the check of a large
    # project slow.
    walk <- which(!vapply(seq_len(n), function(i) sieve_bare_plain(.subset2(x, i)), logical(1)))
    # The names attribute itself: names() would dispatch on the object's class.
    labels <- attr(x, "names", exact = TRUE)
    if (is.null(labels)) {
      labels <- rep("", n)
    }
    for (i in walk) {
      here <- if (nzchar(labels[i])) sprintf("%s$%s", path, labels[i]) else sprintf("%s[[%d]]", path, i)
      out <- c(out, sieve_not_plain(.subset2(x, i), here))
    }
  }
  out
}

# ---- Files SIEVER reads: plain data only ------------------------------------------
#
# A project file, a session input file and a frozen norm reference are R
# serializations, and an unserialized object can hold more than data: an
# environment whose active bindings or unforced promises run code when a
# field is read, a promise inside a list (R 4.4 and later refuse only a
# promise at the top), a closure, or a class whose methods run when the
# object is touched (`$`, names(), length(), format()). So the object is
# looked at, before any field of it is read, only through primitives that
# neither dispatch on a class nor force a promise: typeof() of
# .subset2(), attributes(), isS4(), is.object() and unclass(). No element is
# bound to a variable before its type is known, since a variable bound to a
# promise forces it when it is read.

#' The types plain data may have in a file SIEVER reads
#'
#' @return The [typeof()] names of `NULL`, the atomic vectors and lists.
#' @author Ken Kelley
#' @noRd
sieve_plain_types <- function() {
  c("NULL", "logical", "integer", "double", "complex", "character", "raw", "list")
}

#' Whether class names are classes plain data may carry in a file
#'
#' The classes of data: data frames (with tibble's), factors, dates and
#' times, labelled SPSS and Stata columns (haven, vctrs, hms), the base type
#' names vctrs and lavaan add to a class vector, and the result classes of
#' SIEVER (`sieve_`, `siever_`), DMAR (`dmar_`) and lavaan (`lavaan.`),
#' which a saved analysis record holds. Their methods format and subset data
#' and run nothing stored in the object; any other class could bring a
#' method from another package into play, so it is refused.
#'
#' @param classes A character vector of class names.
#' @return A logical vector, `TRUE` for each name allowed.
#' @author Ken Kelley
#' @noRd
sieve_plain_class_allowed <- function(classes) {
  data_classes <- c(
    "data.frame", "tbl_df", "tbl", "factor", "ordered", "Date", "POSIXct", "POSIXt",
    "difftime", "hms", "AsIs", "table", "haven_labelled", "haven_labelled_spss", "vctrs_vctr",
    "logical", "integer", "double", "numeric", "complex", "character", "list"
  )
  !is.na(classes) & (classes %in% data_classes | grepl("^(sieve|siever|dmar)_|^lavaan[.]", classes))
}

#' Where an object just read from a file holds something that is not plain data
#'
#' The guard every reader of a file runs first, before it reads any field
#' of what it unserialized. The object is passed inside a list of length
#' one, `list(unserialize(bytes))` or `list(readRDS(file))`, so it is never
#' bound to a variable before its type is known. It is then walked level by
#' level, its elements and its attributes alike, looking only through
#' [typeof()] of [.subset2()], [attributes()], [isS4()], [is.object()] and
#' [unclass()], none of which dispatches on a class or forces a promise.
#' Each value must have one of the types `sieve_plain_types()` names and
#' must not be an S4 object, and every class attribute must be a character
#' vector whose classes `sieve_plain_class_allowed()` allows. Environments
#' (with any active binding or promise in them), closures and builtins,
#' promises, R code, external pointers and S4 objects are refused by type,
#' other classes by name; nothing inside a refused value is looked at.
#'
#' @param holder A list of length one whose element is the object read.
#' @param path The name the object is reported under.
#' @param limit The most problems reported.
#' @return The problems, each as `"<path>: type <type>"`,
#'   `"<path>: class <classes>"` or `"<path>: an S4 object"` (empty when the
#'   object is plain data).
#' @author Ken Kelley
#' @noRd
sieve_plain_unserialized <- function(holder, path = "snapshot", limit = 3L) {
  types <- sieve_plain_types()
  if (!identical(typeof(holder), "list") || length(unclass(holder)) != 1L) {
    stop("sieve_plain_unserialized() takes the object read inside a list of length one.", call. = FALSE)
  }
  # The type of element i of a list, read without binding the element to a
  # variable: .subset2() neither dispatches nor forces, nor does typeof().
  type_at <- function(parent, i) typeof(.subset2(parent, i))
  # Whether a value of a plain type needs a look inside: a list, an S4
  # object, or a vector that carries attributes.
  inside <- function(parent, i, type) {
    identical(type, "list") || isS4(.subset2(parent, i)) || !is.null(attributes(.subset2(parent, i)))
  }
  first <- type_at(holder, 1L)
  if (!first %in% types) {
    return(sprintf("%s: type %s", path, first))
  }
  problems <- character(0)
  todo <- list(list(x = .subset2(holder, 1L), path = path))
  while (length(todo) && length(problems) < limit) {
    found <- vector("list", length(todo))
    for (k in seq_along(todo)) {
      # Each value queued has a plain type, so binding it forces nothing.
      x <- todo[[k]]$x
      here <- todo[[k]]$path
      if (is.null(x)) next
      if (isS4(x)) {
        problems <- c(problems, sprintf("%s: an S4 object", here))
        next
      }
      at <- attributes(x)
      if (is.object(x) && is.null(.subset2(at, "class"))) {
        problems <- c(problems, sprintf("%s: an object without a class attribute", here))
        next
      }
      children <- list()
      if (length(at)) {
        an <- names(at)
        at_types <- vapply(seq_along(at), function(j) type_at(at, j), "")
        bad <- !at_types %in% types
        if (any(bad)) {
          problems <- c(problems, sprintf("attr(%s, \"%s\"): type %s", here, an[bad], at_types[bad]))
          next
        }
        j <- match("class", an)
        if (!is.na(j)) {
          if (!identical(at_types[j], "character")) {
            problems <- c(problems, sprintf("%s: a class attribute of type %s", here, at_types[j]))
            next
          }
          classes <- .subset2(at, j)
          foreign <- classes[!sieve_plain_class_allowed(classes)]
          if (length(foreign)) {
            problems <- c(problems, sprintf("%s: class %s", here, paste(foreign, collapse = ", ")))
            next
          }
        }
        for (j in which(at_types != "NULL")) {
          if (inside(at, j, at_types[j])) {
            children <- c(children, list(list(x = .subset2(at, j), path = sprintf("attr(%s, \"%s\")", here, an[j]))))
          }
        }
      }
      if (identical(typeof(x), "list")) {
        n <- length(unclass(x))
        labels <- if (identical(type_at(at, "names"), "character")) .subset2(at, "names") else character(0)
        if (length(labels) != n) {
          labels <- rep("", n)
        }
        labels[is.na(labels)] <- ""
        where <- sprintf("%s[[%d]]", here, seq_len(n))
        named <- nzchar(labels)
        where[named] <- sprintf("%s$%s", here, labels[named])
        el_types <- vapply(seq_len(n), function(j) type_at(x, j), "")
        bad <- !el_types %in% types
        if (any(bad)) {
          problems <- c(problems, sprintf("%s: type %s", where[bad], el_types[bad]))
          next
        }
        deep <- which(el_types != "NULL")
        deep <- deep[vapply(deep, function(j) inside(x, j, el_types[j]), logical(1))]
        children <- c(children, lapply(deep, function(j) list(x = .subset2(x, j), path = where[j])))
      }
      found[[k]] <- children
    }
    todo <- unlist(found, recursive = FALSE)
  }
  utils::head(problems, limit)
}

#' The problems of `sieve_plain_unserialized()` in one phrase
#'
#' @param problems Its value.
#' @return The problems separated by semicolons.
#' @author Ken Kelley
#' @noRd
sieve_plain_problems_text <- function(problems) {
  paste(problems, collapse = "; ")
}

#' Whether an object is plain data without a look inside it
#'
#' `NULL`, or an atomic vector that is not an S4 object and whose
#' attributes are only names, a class, levels, dimensions, dimension names
#' or other atomic vectors with names at most (the attributes of a data
#' column, a factor, a matrix, or an SPSS column with its labels). Anything
#' else is walked by `sieve_not_plain()`.
#'
#' @param x Any object.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_bare_plain <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (!is.atomic(x) || isS4(x)) {
    return(FALSE)
  }
  at <- attributes(x)
  if (is.null(at)) {
    return(TRUE)
  }
  for (a in at) {
    if (is.null(a)) next
    if (!is.atomic(a) || isS4(a)) {
      return(FALSE)
    }
    inner <- attributes(a)
    if (!is.null(inner) && !all(names(inner) %in% c("names", "class", "dim", "dimnames"))) {
      return(FALSE)
    }
  }
  TRUE
}

#' Keep the plain data of an object
#'
#' Removes, from a list and the lists inside it, every element that is not
#' plain data (see `sieve_not_plain()`). Used for the analysis registry, so
#' that a record never takes a fitted engine object into a snapshot.
#'
#' @param x Any object.
#' @return `x` without the elements that are not plain data, or `NULL` when
#'   `x` itself is not plain data.
#' @author Ken Kelley
#' @noRd
sieve_keep_plain <- function(x) {
  if (is.null(x) || !length(sieve_not_plain(x))) {
    return(x)
  }
  # Something in x is not plain data. A plain list is kept without that
  # part; anything else (the object itself, a data frame, a vector with
  # such an attribute) is left out whole.
  if (isS4(x) || !is.list(x) || is.data.frame(x) || is.function(x) || is.environment(x) ||
    is.language(x)) {
    return(NULL)
  }
  kept <- lapply(x, sieve_keep_plain)
  keep <- !vapply(kept, is.null, logical(1)) | vapply(x, is.null, logical(1))
  at <- attributes(x)
  out <- kept[keep]
  for (name in setdiff(names(at), "names")) {
    if (!length(sieve_not_plain(at[[name]]))) {
      attr(out, name) <- at[[name]]
    }
  }
  out
}

#' The identity of a roles table
#'
#' @param roles A roles table, or `NULL`.
#' @return A hash string, or `NA_character_` for `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_roles_key <- function(roles) {
  if (is.null(roles)) {
    return(NA_character_)
  }
  sieve_hash(list(variable = as.character(roles$variable), role = as.character(roles$role)))
}

#' The identity of the whole item ledger
#'
#' `sieve_meta_key()` covers the columns that change an analysis. A saved
#' project also has to give back the labels, the wordings, the response
#' bounds and the notes, so its check covers every column.
#'
#' @param meta The item ledger, or `NULL`.
#' @return A hash string, or `NA_character_` for `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_ledger_key <- function(meta) {
  if (is.null(meta)) {
    return(NA_character_)
  }
  sieve_hash(list(
    names = names(meta), n = nrow(meta),
    columns = unname(lapply(meta, sieve_plain_values))
  ))
}

#' The state fields a snapshot keeps under their own names
#'
#' Every one is kept under its own name but `estimator_choice`, the study's
#' estimator setting, which the snapshot holds as `study_estimator_choice`:
#' a snapshot field whose name began with `estimator` would be returned by
#' `snapshot$estimator` (R's partial matching of `$` on a list) whenever
#' the `estimator` field itself is absent.
#'
#' @return A character vector of field names of the shared state.
#' @author Ken Kelley
#' @noRd
sieve_project_fields <- function() {
  # id_vars is a schema field, derived from roles, that project files hold.
  c(names(Filter(function(field) isTRUE(field$primary), sieve_state_fields())), "id_vars")
}

#' The study's estimator setting a snapshot holds
#'
#' A project holds the setting as `study_estimator_choice`. A saved study
#' may instead hold it among the additional fields
#' (`other$estimator_choice`) or hold none, and then its study follows the
#' item types.
#'
#' @param snapshot A snapshot.
#' @return One of `sieve_estimator_choices()`.
#' @author Ken Kelley
#' @noRd
sieve_snapshot_choice <- function(snapshot) {
  saved <- snapshot[["study_estimator_choice"]] %||% snapshot[["other"]][["estimator_choice"]]
  valid <- unname(sieve_estimator_choices())
  if (is.character(saved) && length(saved) == 1L && !is.na(saved) && saved %in% valid) saved else "auto"
}

#' Does a snapshot record MLR with FIML under Follow the item types?
#'
#' The study's estimator setting `"auto"` (Follow the item types) resolves
#' continuous items to ML with FIML, DMAR's default
#' (`sieve_estimator_resolve()`). A saved study may hold the setting
#' `"auto"` (or none, which reads as `"auto"`) together with the estimator
#' its confirmation recorded, `"MLR with FIML"`: a pair `"auto"` cannot
#' produce, since it gives MLR to nothing, so the pair identifies such a
#' study whatever its SIEVER or schema version. The study's models were
#' fit under MLR. It is read as such only when its included items are all
#' continuous, the items to which `"auto"` gives ML with FIML; a study whose
#' included items are ordered or binary resolves to WLSMV and is left as
#' saved.
#'
#' @param snapshot A snapshot.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_snapshot_legacy_auto <- function(snapshot) {
  if (!is.list(snapshot) || !identical(sieve_snapshot_choice(snapshot), "auto")) {
    return(FALSE)
  }
  recorded <- snapshot$estimator
  if (!(is.character(recorded) && length(recorded) == 1L && identical(recorded, "MLR with FIML"))) {
    return(FALSE)
  }
  meta <- snapshot$meta
  if (!is.data.frame(meta) || !all(c("type", "status") %in% names(meta))) {
    return(FALSE)
  }
  types <- as.character(meta$type[meta$status == "active"])
  if (!length(types)) {
    return(FALSE)
  }
  resolved <- sieve_estimator_resolve(types, choice = "auto")
  identical(resolved$estimator, "ML") && identical(resolved$missing, "fiml")
}

#' The step note of a study kept at MLR with FIML on opening
#'
#' The note `sieve_keep_legacy_mlr()` logs, which the session script finds
#' in a study's log (`sieve_replay_script()`) to say, ahead of the recorded
#' steps, what a recorded `estimator = "auto"` meant. The script matches
#' these words in saved logs, so they are fixed.
#'
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_legacy_auto_note <- function() {
  "Kept the study's estimator as MLR with FIML (saved when Follow the item types meant MLR with FIML)"
}

#' The note shown for a study kept at MLR with FIML on opening
#'
#' @return One paragraph, for the Ingest tab's step 4.
#' @author Ken Kelley
#' @noRd
sieve_legacy_auto_text <- function() {
  paste(
    "This study records its models as fit under MLR with FIML (robust standard errors and a scaled",
    "test statistic) with the study's estimator setting Follow the item types, which gives continuous",
    "items ML with FIML, DMAR's default. So that nothing saved is shown as if it were fit another way",
    "and nothing is refit unasked, the study's setting is MLR with FIML, recorded as your choice. To",
    "use ML with FIML instead, choose Use ML with FIML below (or Follow the item types in the",
    "Estimator control above); the models are then fit under ML with FIML when you next fit them,",
    "with normal-theory standard errors and test statistic."
  )
}

#' Keep MLR with FIML for a study that records it under Follow the item types
#'
#' For a snapshot `sieve_snapshot_legacy_auto()` recognizes: the study's
#' estimator setting is put to `"mlr"` (MLR with FIML), the estimator its
#' models were fit with, and the estimator recorded with the confirmation
#' stays `"MLR with FIML"`, so the saved results, the pages that refit them
#' and the recorded R agree about how they were fit; nothing is refit. The
#' change is logged as a step of the study (with what a recorded SIEVER
#' call with `estimator = "auto"` meant), and `state$project$estimator_kept`
#' holds the note the Ingest tab shows once, beside the Estimator control,
#' with the one click that takes Follow the item types (ML with FIML)
#' (`sieve_legacy_auto_release()`). Called by `sieve_project_restore()`, so
#' every route that brings a saved study into a session (Open project, the
#' scripted replay, the API) reads it the same way.
#'
#' @param state The shared state store, after the snapshot was restored.
#' @param snapshot The snapshot restored.
#' @return Invisibly, `TRUE` when the setting was kept as MLR with FIML.
#' @author Ken Kelley
#' @noRd
sieve_keep_legacy_mlr <- function(state, snapshot) {
  if (!sieve_snapshot_legacy_auto(snapshot)) {
    return(invisible(FALSE))
  }
  shiny::isolate({
    state$estimator_choice <- "mlr"
    state$estimator <- "MLR with FIML"
    sieve_log(
      state,
      sieve_legacy_auto_note(),
      c(
        "# This study was saved with the study's estimator setting \"auto\" (Follow the item types) and",
        "# records its models as fit under MLR with FIML. \"auto\" gives continuous items ML with FIML",
        "# (DMAR's default), so the setting is recorded as \"mlr\" (MLR with FIML), the estimator the",
        "# study was fit with; nothing was refit.",
        "# A SIEVER call recorded above with estimator = \"auto\" was fit under MLR with FIML; to rerun",
        "# it as it was fit, pass estimator = \"mlr\". The lavaan and DMAR calls recorded above name",
        "# their estimator and reproduce as recorded.",
        "# To use ML with FIML, set the study's Estimator control (Ingest tab, step 4) to Follow the item types."
      )
    )
    project <- state$project %||% list()
    project$estimator_kept <- list(from = "auto", kept = "mlr", text = sieve_legacy_auto_text())
    state$project <- project
  })
  invisible(TRUE)
}

#' Take Follow the item types for a study kept at MLR with FIML on opening
#'
#' The one click the Ingest tab offers with the note of
#' `sieve_keep_legacy_mlr()`: the study's estimator setting becomes
#' `"auto"` (Follow the item types: ML with FIML for continuous items), the
#' estimator recorded with a confirmation follows, the change is logged,
#' and the note is cleared. A model page refits under the new setting only
#' when its fit is requested, as after any change of the setting.
#'
#' @param state The shared state store.
#' @return Invisibly, the `sieve_estimator_resolve()` result for the
#'   included items, or `NULL` when no study is loaded.
#' @author Ken Kelley
#' @noRd
sieve_legacy_auto_release <- function(state) {
  shiny::isolate({
    project <- state$project %||% list()
    project$estimator_kept <- NULL
    state$project <- project
    old <- sieve_estimator_effective(state$estimator_choice %||% "auto")
    state$estimator_choice <- "auto"
    meta <- state$meta
    e <- if (is.null(meta)) NULL else {
      sieve_estimator_resolve(meta$type[meta$status == "active"], n = NROW(state$items), choice = "auto")
    }
    if (isTRUE(state$confirmed) && !is.null(e)) {
      state$estimator <- e$label
    }
    sieve_log(
      state,
      sprintf("Set the study's estimator: %s", sieve_estimator_choice_text("auto")),
      c(
        sprintf("# The study's estimator setting (Ingest tab, step 4) is now \"auto\" (it was \"%s\").", old),
        "# Every model tab follows it unless its own Estimator control is set otherwise; the",
        "# code each model records names the estimator it was fit with.",
        if (!is.null(e)) sprintf("# With the included items: %s on a %s matrix.", e$label, e$basis)
      )
    )
    invisible(e)
  })
}

#' The content key of everything in a snapshot that has no key of its own
#'
#' The data as loaded, the item data, the ledger and the role assignments
#' carry their own keys. This key covers the rest of what a project holds:
#' the dataset name, the theory map, the descriptive columns of the roles
#' table, the non item columns, the confirmation and the estimator, the
#' construct basis, the thresholds, the palette, the transformation
#' records, the analysis registry, the code log, the ledger rows held aside
#' for columns that left the item pool, the study's estimator setting, and
#' the additional fields. A change to any of them after the snapshot was
#' taken changes the key. The estimator setting enters the key only when
#' the snapshot holds it, so the key of a project that does not hold the
#' setting is the key it was saved with.
#'
#' @param snapshot A snapshot (any list with these fields).
#' @return A hash string.
#' @author Ken Kelley
#' @noRd
sieve_content_key <- function(snapshot) {
  roles <- snapshot$roles
  described <- if (is.data.frame(roles)) {
    cols <- intersect(c("proposed", "reason", "numeric", "n_distinct", "n_missing", "kind"), names(roles))
    unname(lapply(roles[cols], sieve_plain_values))
  }
  parts <- list(
    dataset_name = snapshot$dataset$name,
    theory = snapshot$theory,
    roles_described = described,
    id_vars = as.character(snapshot$id_vars),
    confirmed = snapshot$confirmed,
    estimator = snapshot$estimator,
    construct_basis = snapshot$construct_basis,
    thresholds = snapshot$thresholds,
    palette = snapshot$palette,
    transforms = snapshot$transforms,
    analyses = snapshot$analyses,
    codelog = snapshot$codelog,
    held_rows = snapshot$held_rows,
    other = snapshot$other
  )
  if (!is.null(snapshot[["study_estimator_choice"]])) {
    parts$study_estimator_choice <- snapshot[["study_estimator_choice"]]
  }
  sieve_hash(parts)
}

#' Copy the session into a plain project snapshot
#'
#' The snapshot is one plain list: nothing in it is a reactive object, an
#' environment or a fitted model, so it can be saved, read in another
#' session and compared. It holds the schema version and the SIEVER version
#' that wrote it, the data as loaded, the analytic item data, the item
#' ledger, the theory map, the variable roles, the confirmation flag and
#' the estimator, the study's estimator setting, the construct basis, the thresholds, the transformation
#' records, the analysis registry (the records, never a fitted object), the
#' code log, the ledger rows held aside for columns that left the item
#' pool, the revision, and content keys that the reader checks: of the data,
#' the ledger and the roles, and one of everything else
#' (`sieve_content_key()`). Any other field of the state that is plain data
#' is kept under `other`, so a field added to the state later is not lost;
#' what could not be kept is named in `dropped`.
#'
#' @param state The shared state store (read under [shiny::isolate()]).
#' @return A list of class `sieve_project` (see the fields above).
#' @author Ken Kelley
#' @noRd
sieve_project_snapshot <- function(state) {
  shiny::isolate({
    all_fields <- shiny::reactiveValuesToList(state)
    raw <- all_fields$raw
    items <- all_fields$items
    meta <- all_fields$meta
    if (is.null(raw) || is.null(items) || is.null(meta)) {
      sieve_project_stop("No data are loaded, so there is no project to save.")
    }
    roles <- sieve_roles_table(raw, items, all_fields$roles)
    analyses <- lapply(all_fields$analyses %||% list(), sieve_keep_plain)
    # The correlation cache (state$cache) is recomputed on demand and is not
    # part of the scientific record, so it is not saved; nor is the session's
    # fit store (state$fit_store), which holds fitted engine objects.
    other <- all_fields[setdiff(names(all_fields), c(sieve_project_fields(), sieve_state_exclusions("persisted")))]
    dropped <- c(
      sieve_not_plain(all_fields$analyses, "analyses"),
      sieve_not_plain(other, "other")
    )
    other <- sieve_keep_plain(other) %||% list()
    snapshot <- list(
      format = "SIEVER project",
      schema_version = sieve_project_schema(),
      siever_version = sieve_version(),
      saved = list(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE),
        r = paste(R.version$major, R.version$minor, sep = "."),
        engine_identity = sieve_engine_identity()
      ),
      dataset = list(
        name = all_fields$dataset_name,
        id = all_fields$dataset_id %||% NA_character_,
        seq = as.integer(all_fields$dataset_seq %||% 0L)
      ),
      raw = raw,
      items = items,
      meta = meta,
      theory = all_fields$theory,
      roles = roles,
      id_vars = roles$variable[roles$role != "item"],
      confirmed = sieve_confirmation_status(state)$confirmed,
      estimator = all_fields$estimator,
      # The study's estimator setting (the Estimator control in step 4 of
      # the Ingest tab).
      study_estimator_choice = sieve_snapshot_choice(list(study_estimator_choice = all_fields$estimator_choice)),
      construct_basis = all_fields$construct_basis %||% "theoretical",
      thresholds = all_fields$thresholds,
      palette = all_fields$palette %||% "nd",
      transforms = all_fields$transforms %||% list(),
      analyses = analyses,
      codelog = as.character(all_fields$codelog %||% character(0)),
      held_rows = all_fields$held_rows %||% list(),
      other = other,
      revision = sieve_revision(state),
      keys = list(
        raw = sieve_data_key(raw),
        items = sieve_data_key(items),
        meta = sieve_meta_key(meta),
        ledger = sieve_ledger_key(meta),
        roles = sieve_roles_key(roles)
      ),
      dropped = dropped
    )
    snapshot$keys$content <- sieve_content_key(snapshot)
    structure(snapshot, class = "sieve_project")
  })
}

#' The measurement levels a ledger may record
#'
#' @return A character vector: the three levels the grid offers and the two
#'   that detection reports for a column that cannot be an item.
#' @author Ken Kelley
#' @noRd
sieve_type_levels <- function() {
  c("continuous", "ordered", "binary", "constant", "text")
}

#' What is wrong with a project snapshot
#'
#' The checks every snapshot passes before it is written, after it is read
#' and before it is restored: it is a SIEVER project of the current schema,
#' it holds only plain data, its parts have the shapes the session needs
#' and agree with one another, its values keep the rule of the
#' transformation service (each item is at its values as loaded or at their
#' single reflection within its stored bounds, and an item that is not at
#' its values as loaded, or that the ledger records as reverse scored, has a
#' transformation record in the dataset the project holds), its ledger uses
#' the type vocabulary, a confirmed project names its estimator, the
#' study's estimator setting (when it holds one) is one of
#' `sieve_estimator_choices()`, its thresholds are numbers, its records name items of the data and datasets
#' the session loaded, and the data, the ledger, the roles and everything
#' else are the ones whose content keys it carries.
#'
#' @param snapshot A candidate snapshot.
#' @return A character vector of problems, empty when there are none.
#' @author Ken Kelley
#' @noRd
sieve_project_check <- function(snapshot) {
  if (!is.list(snapshot) || !identical(snapshot$format, "SIEVER project")) {
    return("it is not a SIEVER project")
  }
  version <- snapshot$schema_version
  if (!is.numeric(version) || length(version) != 1L || is.na(version) ||
    version != sieve_project_schema()) {
    return(sprintf("its schema version is not %d", sieve_project_schema()))
  }
  bad <- sieve_not_plain(unclass(snapshot))
  if (length(bad)) {
    return(sprintf(
      "it holds something that is not data (%s)",
      paste(utils::head(bad, 3L), collapse = ", ")
    ))
  }
  problems <- character(0)
  need <- c(
    "raw", "items", "meta", "roles", "id_vars", "confirmed", "transforms", "analyses",
    "codelog", "held_rows", "revision", "keys", "dataset"
  )
  missing <- setdiff(need, names(snapshot))
  if (length(missing)) {
    return(sprintf("it has no %s", paste(missing, collapse = ", ")))
  }
  raw <- snapshot$raw
  items <- snapshot$items
  meta <- snapshot$meta
  roles <- snapshot$roles
  if (!is.data.frame(raw) || !is.data.frame(items) || !is.data.frame(meta) || !is.data.frame(roles)) {
    return("its data, item data, ledger or roles are not tables")
  }
  if (nrow(raw) != nrow(items)) {
    problems <- c(problems, "the item data and the data as loaded have different numbers of rows")
  }
  if (!all(names(items) %in% names(raw))) {
    problems <- c(problems, "the item data hold a column the data as loaded do not have")
  }
  ledger_cols <- c("item", "label", "construct", "facet", "type", "status", "reverse")
  if (!all(ledger_cols %in% names(meta))) {
    problems <- c(problems, sprintf(
      "the ledger has no %s column",
      paste(setdiff(ledger_cols, names(meta)), collapse = ", ")
    ))
  } else {
    if (anyDuplicated(meta$item) || !setequal(meta$item, names(items))) {
      problems <- c(problems, "the ledger and the item data do not describe the same items")
    }
    if (!all(meta$status %in% c("active", "parked"))) {
      problems <- c(problems, "the ledger holds a status that is neither included nor excluded")
    }
    if (!all(as.character(meta$type) %in% sieve_type_levels())) {
      problems <- c(problems, sprintf(
        "the ledger holds a measurement type that is not one of %s (%s)",
        paste(sieve_type_levels(), collapse = ", "),
        sieve_name_some(as.character(meta$type)[!as.character(meta$type) %in% sieve_type_levels()])
      ))
    }
  }
  if (!all(c("variable", "role") %in% names(roles)) ||
    !identical(as.character(roles$variable), names(raw))) {
    problems <- c(problems, "the roles table does not list the columns of the data")
  } else {
    if (!all(roles$role %in% sieve_role_levels())) {
      problems <- c(problems, "the roles table holds a role that is not one of the roles")
    }
    if (!setequal(roles$variable[roles$role == "item"], names(items))) {
      problems <- c(problems, "the roles table and the item data disagree about which columns are items")
    }
    if (!identical(as.character(snapshot$id_vars), roles$variable[roles$role != "item"])) {
      problems <- c(problems, "the non item columns do not match the roles table")
    }
  }
  if (!is.logical(snapshot$confirmed) || length(snapshot$confirmed) != 1L || is.na(snapshot$confirmed)) {
    problems <- c(problems, "the confirmation flag is not TRUE or FALSE")
  } else if (isTRUE(snapshot$confirmed)) {
    estimator <- snapshot$estimator
    if (!is.character(estimator) || length(estimator) != 1L || is.na(estimator) || !nzchar(estimator)) {
      problems <- c(problems, "the item types are confirmed but no estimator is recorded")
    }
  }
  choice <- snapshot[["study_estimator_choice"]]
  if (!is.null(choice) && !(is.character(choice) && length(choice) == 1L && !is.na(choice) &&
    choice %in% unname(sieve_estimator_choices()))) {
    problems <- c(problems, sprintf(
      "the study's estimator setting is not one of %s",
      paste(unname(sieve_estimator_choices()), collapse = ", ")
    ))
  }
  thresholds <- snapshot$thresholds
  if (!is.null(thresholds)) {
    numbers <- is.list(thresholds) && length(thresholds) > 0L && !is.null(names(thresholds)) &&
      all(nzchar(names(thresholds))) &&
      all(vapply(thresholds, function(v) is.numeric(v) && length(v) == 1L && is.finite(v), logical(1)))
    if (!numbers) {
      problems <- c(problems, "the thresholds are not a named list of numbers")
    }
  }
  if (!is.list(snapshot$transforms) || !is.list(snapshot$analyses) || !is.character(snapshot$codelog)) {
    problems <- c(
      problems,
      "the transformation records, the analysis registry or the code log are not what they should be"
    )
  }
  held <- snapshot$held_rows
  if (!is.list(held) || is.data.frame(held) ||
    (length(held) > 0L && (is.null(names(held)) || any(!nzchar(names(held))) ||
      anyDuplicated(names(held)) > 0L))) {
    problems <- c(problems, "the ledger rows held aside are not a list named by variable")
  } else if (length(held)) {
    good_row <- vapply(names(held), function(v) {
      row <- held[[v]]
      is.data.frame(row) && nrow(row) == 1L && "item" %in% names(row) &&
        identical(as.character(row$item), v) && v %in% names(raw) && !v %in% names(items)
    }, logical(1))
    if (!all(good_row)) {
      problems <- c(problems, sprintf(
        "a ledger row held aside does not describe a non item column of the data (%s)",
        sieve_name_some(names(held)[!good_row])
      ))
    }
  }
  other <- snapshot$other
  # A project without the estimator setting's own field may hold the
  # setting among the additional fields.
  legacy <- if (is.null(snapshot[["study_estimator_choice"]])) "estimator_choice" else character(0)
  if (!is.null(other) && length(other) > 0L &&
    (!is.list(other) || is.null(names(other)) || any(!nzchar(names(other))) ||
      any(names(other) %in% setdiff(sieve_project_fields(), legacy)))) {
    problems <- c(problems, "its additional fields are unnamed or repeat a field the project already holds")
  }
  if (is.list(other)) {
    design <- other$sampling_design %||% list(unit = "independent")
    unit <- if (is.list(design)) design$unit %||% "independent" else NULL
    if (!is.list(design) || !is.character(unit) || length(unit) != 1L || is.na(unit) ||
      !unit %in% c("independent", "clustered", "repeated")) {
      problems <- c(problems, "the sampling design names no supported sampling unit")
    } else if (length(design$cluster) &&
      (length(design$cluster) != 1L || is.na(design$cluster) ||
        (nzchar(design$cluster) && !design$cluster %in% names(raw)))) {
      problems <- c(problems, "the sampling design's identifier is not a column of the loaded data")
    }
    index <- other$case_index %||% seq_len(nrow(raw))
    removed <- other$removed_cases %||% list()
    removed_rows <- if (is.list(removed)) vapply(removed, function(r) {
      if (is.list(r) && is.numeric(r$row) && length(r$row) == 1L && is.finite(r$row)) as.integer(r$row) else NA_integer_
    }, integer(1)) else NA_integer_
    if (!is.numeric(index) || length(index) != nrow(raw) || anyNA(index) ||
      any(index < 1 | index != trunc(index)) || anyDuplicated(index) ||
      anyNA(removed_rows) || anyDuplicated(removed_rows) || any(removed_rows < 1L) ||
      length(intersect(index, removed_rows))) {
      problems <- c(problems, "the current and removed respondent row identities disagree")
    }
    # A validation reservation in force (fct_holdout.R) names rows that are
    # among the removed respondents, each marked with the reservation.
    holdout <- other$holdout %||% list()
    reservation_ok <- is.list(holdout) && all(vapply(holdout, function(h) {
      is.list(h) && is.character(h$id) && length(h$id) == 1L && is.numeric(h$rows) && !anyNA(h$rows) &&
        (!identical(h$status, "reserved") || (is.list(removed) && all(as.integer(h$rows) %in% removed_rows) &&
          all(vapply(removed[match(as.integer(h$rows), removed_rows)], function(r) identical(r$holdout, h$id), logical(1)))))
    }, logical(1)))
    if (!reservation_ok) {
      problems <- c(problems, "the validation reservation names rows that are not held as reserved")
    }
    screen <- other$screening_spec %||% list()
    if (!is.list(screen) || (!is.null(screen$items) &&
      (!is.character(screen$items) || anyNA(screen$items) || any(!screen$items %in% names(raw))))) {
      problems <- c(problems, "the screening definition names items absent from the loaded data")
    }
    policies <- other$scoring_policies %||% list()
    policy_ok <- function(p) {
      if (!is.list(p)) return(FALSE)
      count <- p$min_count %||% 1L
      prop <- p$min_prop %||% 0
      # The score method: absent in a study that records none, one of the
      # operational scores otherwise. A method this version does not
      # know (as from a later version of SIEVER) does not refuse the project:
      # it is kept as saved, read as the mean, and flagged for review by
      # sieve_score_policy_read(). The response minimums and weights are
      # still checked here.
      method_ok <- is.null(p$method) || (is.character(p$method) && length(p$method) == 1L &&
        !is.na(p$method) && nzchar(p$method))
      method_ok &&
        is.numeric(count) && length(count) == 1L && is.finite(count) && count >= 0 && count == trunc(count) &&
        is.numeric(prop) && length(prop) == 1L && is.finite(prop) && prop >= 0 && prop <= 1 &&
        (is.null(p$weights) || (is.numeric(p$weights) && all(is.finite(p$weights)) && all(p$weights >= 0) && any(p$weights > 0)))
    }
    if (!is.list(policies) || (length(policies) && (is.null(names(policies)) || any(!nzchar(names(policies))))) ||
      !all(vapply(policies, policy_ok, logical(1)))) {
      problems <- c(problems, "the construct scoring policies are not valid response and weighting rules")
    }
    revisions <- other$replay_states %||% list()
    steps <- other$replay_steps %||% list()
    operations <- other$replay_operations %||% list()
    if (!is.list(revisions) || !is.list(steps) || !is.list(operations) ||
      any(!unlist(steps, use.names = FALSE) %in% names(revisions))) {
      problems <- c(problems, "the session script refers to an absent input revision")
    } else if (length(revisions)) {
      if (is.null(names(revisions)) || any(!nzchar(names(revisions)))) {
        return(c(problems, "the session input revisions have no content keys"))
      }
      revision_ok <- vapply(names(revisions), function(key) {
        record <- revisions[[key]]
        if (!is.list(record) || !identical(sieve_hash(record), key)) return(FALSE)
        run <- sieve_capture(sieve_replay_expand(record, other$replay_data %||% list()))
        run$ok
      }, logical(1))
      op_ok <- vapply(operations, function(op) {
        if (!is.list(op)) return(FALSE)
        if (isTRUE(op$legacy)) return(is.character(op$note) && length(op$note) == 1L &&
          !is.na(op$note) && is.character(op$code) && !anyNA(op$code) &&
          all(names(op) %in% c("legacy", "note", "code")))
        length(op$kind) == 1L && op$kind %in% c("analysis", "mutation", "import") &&
          length(op$before) == 1L && op$before %in% names(revisions) &&
          length(op$after) == 1L && op$after %in% names(revisions)
      }, logical(1))
      if (!all(revision_ok) || !all(op_ok)) {
        problems <- c(problems, "a session input revision or operation does not match its saved data")
      }
    }
  }
  if (length(problems)) {
    return(problems)
  }

  # The records: each names an item of the data, a known kind of operation
  # and a dataset the saving session loaded.
  seq_saved <- as.integer(snapshot$dataset$seq %||% 0L)
  record_ok <- vapply(seq_along(snapshot$transforms), function(k) {
    r <- snapshot$transforms[[k]]
    id <- suppressWarnings(as.integer(sub("^D", "", as.character(r$dataset_id %||% ""))))
    is.list(r) && is.character(r$item) && length(r$item) == 1L && r$item %in% names(raw) &&
      is.character(r$type) && length(r$type) == 1L && r$type %in% c("reverse", "keying") &&
      is.character(r$dataset_id) && length(r$dataset_id) == 1L &&
      grepl("^D[0-9]+$", r$dataset_id) && !is.na(id) && (is.na(seq_saved) || id <= seq_saved)
  }, logical(1))
  if (!all(record_ok)) {
    problems <- c(problems, sprintf(
      "a transformation record names an item that is not in the data, an operation that does not exist, or a dataset the session never loaded (record %s)",
      sieve_name_some(as.character(which(!record_ok)))
    ))
  }
  if (!all(vapply(snapshot$analyses, is.list, logical(1)))) {
    problems <- c(problems, "the analysis registry holds something that is not a record")
  }

  # The rule of the transformation service: each item is at its values as
  # loaded or at their single reflection within its stored bounds, and an
  # item away from its values as loaded, or recorded as reverse scored, has
  # a record in the dataset the project holds.
  meta_k <- sieve_meta_keying(meta)
  recorded_items <- unique(vapply(
    Filter(function(r) identical(r$dataset_id, snapshot$dataset$id), snapshot$transforms),
    function(r) as.character(r$item), character(1)
  ))
  off_rule <- character(0)
  unrecorded <- character(0)
  for (i in seq_len(nrow(meta_k))) {
    it <- meta_k$item[i]
    if (!it %in% names(raw) || !is.numeric(raw[[it]]) || !is.numeric(items[[it]])) {
      next
    }
    current <- as.numeric(items[[it]])
    loaded <- as.numeric(raw[[it]])
    at_loaded <- identical(current, loaded)
    lo <- as.numeric(meta_k$rev_lo[i])
    hi <- as.numeric(meta_k$rev_hi[i])
    reflected <- !at_loaded && is.finite(lo) && is.finite(hi) && identical(current, (lo + hi) - loaded)
    if (!at_loaded && !reflected) {
      off_rule <- c(off_rule, it)
    } else if ((!at_loaded || isTRUE(as.logical(meta_k$reverse[i]))) && !it %in% recorded_items) {
      unrecorded <- c(unrecorded, it)
    }
  }
  if (length(off_rule)) {
    problems <- c(problems, sprintf(
      "the values of %s are neither the values as loaded nor their reflection within the stored response bounds",
      sieve_name_some(off_rule)
    ))
  }
  if (length(unrecorded)) {
    problems <- c(problems, sprintf(
      "%s %s reverse scored by the ledger or by the values, and no transformation record of the dataset says so",
      sieve_name_some(unrecorded), if (length(unrecorded) == 1L) "is" else "are"
    ))
  }

  now <- list(
    raw = sieve_data_key(raw),
    items = sieve_data_key(items),
    meta = sieve_meta_key(meta),
    ledger = sieve_ledger_key(meta),
    roles = sieve_roles_key(roles),
    content = sieve_content_key(snapshot)
  )
  for (part in names(now)) {
    if (!identical(snapshot$keys[[part]], now[[part]])) {
      problems <- c(problems, sprintf(
        "its %s are not the ones it was saved with (the file was changed or damaged after it was written)",
        c(
          raw = "data as loaded", items = "item data", meta = "ledger entries",
          ledger = "ledger entries", roles = "variable roles",
          content = "records, settings and session script"
        )[[part]]
      ))
    }
  }
  if (!identical(snapshot$revision$data_key, now$items) ||
    !identical(snapshot$revision$meta_key, now$meta)) {
    problems <- c(problems, "its revision does not describe its own item data and ledger")
  }
  claim_state <- snapshot$other %||% list()
  claim_state$codelog <- snapshot$codelog
  claims <- sieve_claims_check(claim_state)
  if (nrow(claims) && any(claims$intact %in% FALSE)) {
    problems <- c(problems, "a frozen claim or evaluation does not match its recorded content key")
  }
  unique(problems)
}

#' The upgrade steps between project schema versions
#'
#' A list of functions named by the schema version each one upgrades
#' *from*: the function named `"1"` takes a schema 1 snapshot and returns a
#' schema 2 snapshot, and so on. Each step is small and is tested on its
#' own; a file two versions old passes through both in order.
#'
#' Schema 2 adds `held_rows`, the ledger rows kept aside for columns that
#' left the item pool, and `keys$content`, the key of everything the other
#' keys do not cover. A schema 1 file gets an empty
#' `held_rows` and a content key of what it holds.
#'
#' @return A named list of functions.
#' @author Ken Kelley
#' @noRd
sieve_project_steps <- function() {
  list(
    "1" = function(snapshot) {
      snapshot$held_rows <- snapshot$held_rows %||% list()
      snapshot$keys$content <- sieve_content_key(snapshot)
      snapshot$schema_version <- 2L
      snapshot
    },
    "2" = function(snapshot) {
      # Validate the schema 2 content before changing its layout or hashes.
      # Migration must never bless a changed scientific record.
      if (!identical(snapshot$keys$content, sieve_content_key(snapshot))) {
        sieve_project_stop("The project's records, settings or script do not match their saved content key.")
      }
      other <- snapshot$other %||% list()
      fresh <- shiny::isolate(shiny::reactiveValuesToList(sieve_new_state()))
      defaults <- setdiff(names(fresh), c(sieve_project_fields(), sieve_state_exclusions("persisted")))
      for (field in setdiff(defaults, names(other))) other[field] <- fresh[field]
      legacy <- other$replay_states %||% list()
      if (length(legacy) && any(vapply(legacy, function(x) !is.null(x$raw), logical(1)))) {
        temp <- sieve_new_state()
        mapping <- character(0)
        for (key in names(legacy)) {
          x <- legacy[[key]]
          x$removed_cases <- x$removed_cases %||% list()
          mapping[key] <- sieve_replay_store(temp, x)
        }
        other$replay_data <- shiny::isolate(temp$replay_data)
        other$replay_states <- shiny::isolate(temp$replay_states)
        other$replay_steps <- lapply(other$replay_steps %||% list(), function(key) unname(mapping[[key]]))
      }
      other$replay_operations <- other$replay_operations %||% list()
      other$replay_data <- other$replay_data %||% list()
      snapshot$other <- other
      snapshot$schema_version <- 3L
      snapshot$keys$content <- sieve_content_key(snapshot)
      snapshot
    }
  )
}

#' Bring a project snapshot to the current schema, one version at a time
#'
#' A snapshot of a lower schema version is upgraded one schema version at
#' a time, each step by the function registered for it, so every step is a
#' small, testable change and a file two versions old passes through both
#' steps in order. A snapshot of a schema newer than this SIEVER knows is
#' refused: reading it as if it were the current schema could silently drop
#' or misread what the newer version added.
#'
#' @param snapshot A snapshot (any schema version).
#' @param to The schema version to reach.
#' @param steps The upgrade steps (see `sieve_project_steps()`).
#' @return The snapshot at schema `to`, with `migrated_from` listing the
#'   versions it passed through when any step was taken.
#' @author Ken Kelley
#' @noRd
sieve_project_migrate <- function(snapshot, to = sieve_project_schema(),
                                  steps = sieve_project_steps()) {
  if (!is.list(snapshot) || !identical(snapshot$format, "SIEVER project")) {
    sieve_project_stop("This is not a SIEVER project.")
  }
  version <- snapshot$schema_version
  if (!is.numeric(version) || length(version) != 1L || is.na(version) || version < 1 ||
    version != trunc(version)) {
    sieve_project_stop("The project does not say which schema version it was saved in, so it cannot be opened.")
  }
  version <- as.integer(version)
  to <- as.integer(to)
  if (version > to) {
    sieve_project_stop(
      paste(
        "The project was saved by a newer SIEVER (project schema %d; this version reads up to",
        "schema %d). Update SIEVER to open it."
      ),
      version, to
    )
  }
  while (version < to) {
    step <- steps[[as.character(version)]]
    if (!is.function(step)) {
      sieve_project_stop(
        "There is no upgrade from project schema %d to %d, so the project cannot be opened.",
        version, version + 1L
      )
    }
    snapshot <- step(snapshot)
    if (!is.list(snapshot) || !identical(as.integer(snapshot$schema_version), version + 1L)) {
      sieve_project_stop(
        "The upgrade from project schema %d did not produce schema %d.",
        version, version + 1L
      )
    }
    snapshot$migrated_from <- c(snapshot$migrated_from, version)
    version <- version + 1L
  }
  snapshot
}

# ---- The project file ---------------------------------------------------------
#
# A project file is a small container: a header of text lines, then the
# payload, which is the serialized snapshot, compressed. The header names
# the format and the container version, the schema version of the snapshot,
# how the payload is encoded and how long it is, and a digest of its bytes.
# The reader verifies the length and the digest of the raw payload bytes
# before anything is decompressed or unserialized, so a damaged file is
# refused at the door and no damaged byte stream reaches R's unserializer
# (which, given one, can return a changed project, crash, or hang). A file
# that is a plain RDS of the snapshot, without the container, is also read,
# through the checks that a snapshot passes, but without that protection.

#' The project container version this SIEVER writes
#'
#' The container version belongs to the file layout around the snapshot
#' (the header and the payload encoding); the schema version belongs to the
#' snapshot itself. Version 0 stands for a project file that is a plain RDS
#' of the snapshot, which carries no header.
#'
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_project_container <- function() {
  1L
}

#' The digest of a vector of bytes
#'
#' MD5 through [tools::md5sum()], which every supported R has (SHA-256 in
#' `tools` needs R 4.5). The digest guards against damage, not against an
#' adversary: a changed byte changes it. The name of the algorithm is
#' written into the file header, so a later change of algorithm leaves older
#' files readable.
#'
#' @param bytes A raw vector.
#' @return A length one character string of hexadecimal digits.
#' @author Ken Kelley
#' @noRd
sieve_bytes_digest <- function(bytes) {
  path <- tempfile("sieve_digest_")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::md5sum(path))
}

#' Encode a project snapshot as the bytes of a project file
#'
#' @param snapshot A snapshot (checked by the caller).
#' @return A raw vector: the header, a blank line, then the payload.
#' @author Ken Kelley
#' @noRd
sieve_project_encode <- function(snapshot) {
  plain <- serialize(snapshot, connection = NULL, ascii = FALSE, xdr = TRUE, version = 3L)
  payload <- memCompress(plain, type = "gzip")
  # The schema line describes the snapshot; the reader takes the schema from
  # the snapshot itself once the content is verified. A snapshot without one
  # (which the writer refuses before encoding) is written as schema 0.
  version <- snapshot$schema_version
  schema <- if (is.numeric(version) && length(version) == 1L && !is.na(version)) as.integer(version) else 0L
  header <- c(
    sprintf("SIEVER project container %d", sieve_project_container()),
    sprintf("schema %d", schema),
    sprintf("payload gzip %d", length(payload)),
    sprintf("digest md5 %s", sieve_bytes_digest(payload)),
    "",
    ""
  )
  c(charToRaw(paste(header, collapse = "\n")), payload)
}

#' Refuse a project file whose content is not plain data
#'
#' Runs `sieve_plain_unserialized()` on what a project file held, before
#' any field of it is read.
#'
#' @param holder `list(<the object unserialized>)`.
#' @return `TRUE`, invisibly; otherwise stops with a `sieve_project_error`
#'   that names where the file holds what, and its type or class.
#' @author Ken Kelley
#' @noRd
sieve_project_plain <- function(holder) {
  bad <- sieve_plain_unserialized(holder, "snapshot")
  if (length(bad)) {
    sieve_project_stop(
      paste(
        "The file holds something that is not plain data (%s), so it was not opened.",
        "Project files hold only data; nothing in the file was run."
      ),
      sieve_plain_problems_text(bad)
    )
  }
  invisible(TRUE)
}

#' Decode the bytes of a project file
#'
#' Reads the header, refuses one that is not in the form SIEVER writes or
#' that names a container version this SIEVER does not know, checks that
#' the payload has the length the header gives and the digest the header
#' gives, and only then decompresses and unserializes it. What was
#' unserialized is refused unless it is plain data (`sieve_project_plain()`)
#' before any field of it is read, the schema version included. Bytes that
#' do not begin with the container's first line are not a container at all,
#' and `NULL` is returned so the caller can read them as a plain RDS.
#'
#' @param bytes The bytes of the file.
#' @return `NULL` when the bytes are not a container, else a list with
#'   `snapshot` (plain data, otherwise unchecked), `container` (the version)
#'   and `schema` (the schema version the header names).
#' @author Ken Kelley
#' @noRd
sieve_project_decode <- function(bytes) {
  magic <- charToRaw("SIEVER project container ")
  if (!is.raw(bytes) || length(bytes) < length(magic) || !identical(bytes[seq_along(magic)], magic)) {
    return(NULL)
  }
  damaged <- function(what) {
    sieve_project_stop(
      paste(
        "The file begins like a SIEVER project, but %s, so it was not opened. The file was",
        "changed or damaged after it was written: save the project again from the session that",
        "made it."
      ),
      what
    )
  }
  # The header ends at the first blank line, within its first few hundred bytes.
  limit <- min(length(bytes), 512L)
  newline <- which(bytes[seq_len(limit)] == as.raw(10L))
  blank <- newline[newline < limit & bytes[pmin(newline + 1L, length(bytes))] == as.raw(10L)]
  if (!length(blank)) {
    damaged("its header does not end")
  }
  end <- blank[1]
  text <- sieve_capture(rawToChar(bytes[seq_len(end - 1L)]))
  if (!text$ok) {
    damaged("its header is not text")
  }
  lines <- strsplit(text$value, "\n", fixed = TRUE)[[1]]
  forms <- c(
    container = "^SIEVER project container ([0-9]+)$",
    schema = "^schema ([0-9]+)$",
    payload = "^payload (gzip|none) ([0-9]+)$",
    digest = "^digest (md5|sha256) ([0-9a-f]{32}|[0-9a-f]{64})$"
  )
  if (length(lines) != length(forms) || !all(mapply(grepl, forms, lines))) {
    damaged("its header is not in the form SIEVER writes")
  }
  field <- function(k, which) regmatches(lines[k], regexec(forms[[k]], lines[k]))[[1]][which + 1L]
  container <- as.integer(field(1L, 1L))
  if (is.na(container) || container < 1L) {
    damaged("its header names no project file version")
  }
  if (container > sieve_project_container()) {
    sieve_project_stop(
      paste(
        "The project was saved by a newer SIEVER (project file version %s; this version reads",
        "up to version %d). Update SIEVER to open it."
      ),
      field(1L, 1L), sieve_project_container()
    )
  }
  schema <- as.integer(field(2L, 1L))
  encoding <- field(3L, 1L)
  declared <- suppressWarnings(as.numeric(field(3L, 2L)))
  algorithm <- field(4L, 1L)
  digest <- field(4L, 2L)
  payload <- if (end + 1L < length(bytes)) bytes[(end + 2L):length(bytes)] else raw(0)
  if (is.na(declared) || length(payload) != declared) {
    damaged(sprintf(
      "it holds %d bytes of content where its header says %s", length(payload),
      field(3L, 2L)
    ))
  }
  computed <- if (identical(algorithm, "sha256")) {
    if (!exists("sha256sum", envir = asNamespace("tools"))) {
      sieve_project_stop(
        "The project's digest is SHA-256, which this R (%s) cannot verify. Open it with R 4.5 or later.",
        paste(R.version$major, R.version$minor, sep = ".")
      )
    }
    path <- tempfile("sieve_digest_")
    on.exit(unlink(path), add = TRUE)
    writeBin(payload, path)
    unname(get("sha256sum", envir = asNamespace("tools"))(path))
  } else {
    sieve_bytes_digest(payload)
  }
  if (!identical(tolower(computed), digest)) {
    damaged("its content does not match the digest in its header")
  }
  plain <- if (identical(encoding, "gzip")) {
    sieve_capture(memDecompress(payload, type = "gzip"))
  } else {
    list(ok = TRUE, value = payload)
  }
  if (!plain$ok) {
    damaged("its content could not be decompressed")
  }
  # Held in a list, so that what was unserialized is checked for plain data
  # before any field of it is read (sieve_plain_unserialized()).
  read <- sieve_capture(list(unserialize(plain$value)))
  if (!read$ok) {
    damaged("its content could not be read")
  }
  sieve_project_plain(read$value)
  snapshot <- .subset2(read$value, 1L)
  # The schema line is outside the digest, so an edited one is caught here:
  # the content's own schema version, read the way the writer wrote the line
  # (a snapshot without one is written as schema 0), has to be the header's.
  if (is.list(snapshot)) {
    held <- snapshot$schema_version
    stated <- if (is.numeric(held) && length(held) == 1L && !is.na(held)) as.integer(held) else 0L
    if (!identical(stated, schema)) {
      damaged("its header names a schema version other than the one its content holds")
    }
  }
  list(snapshot = snapshot, container = container, schema = schema)
}

#' Write a Project Snapshot to a File
#'
#' The snapshot is checked first, encoded as a project file
#' (`sieve_project_encode()`), written to a temporary file beside the target
#' and moved into place, so a failed write never leaves a partial project
#' file under the target name.
#'
#' @param snapshot A snapshot from `sieve_project_snapshot()`.
#' @param file The path to write.
#' @return The path, invisibly.
#' @author Ken Kelley
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' project <- sieve_project_read(path)
#' copy <- tempfile("holzinger_copy_", fileext = ".siever.rds")
#' sieve_project_write(project, copy)
#' identical(sieve_project_read(copy)$meta, project$meta)
#' @export
sieve_project_write <- function(snapshot, file) {
  problems <- sieve_project_check(snapshot)
  if (length(problems)) {
    sieve_project_stop("The project was not saved: %s.", paste(problems, collapse = "; "))
  }
  bytes <- sieve_project_encode(snapshot)
  partial <- tempfile("siever_project_", tmpdir = dirname(file), fileext = ".part")
  on.exit(unlink(partial), add = TRUE)
  writeBin(bytes, partial)
  if (!file.rename(partial, file)) {
    sieve_project_stop(
      "The project could not be written to %s. Choose another folder or file name and save again.",
      file
    )
  }
  invisible(file)
}

#' Read a Project Snapshot From a File
#'
#' Reads the file as a project container (`sieve_project_decode()`), which
#' verifies the digest of the content before anything is unserialized, or,
#' for a file that is not a container, as a plain RDS. Refuses anything
#' that is not a SIEVER project or that holds more than plain data,
#' upgrades a lower schema version with `sieve_project_migrate()`, and
#' checks the result with `sieve_project_check()`.
#'
#' @details
#' What was unserialized is checked type by type before any field of it is
#' read, through primitives that neither dispatch methods nor force
#' promises (`typeof()` of each element and attribute, `attributes()`,
#' `isS4()`). Every value, at any depth and in any attribute, must be
#' `NULL`, a logical, integer, double, complex, character or raw vector, or
#' a list, and must not be an S4 object; every class must be a class of data
#' (data frames, factors, dates and times, haven's labelled SPSS and Stata
#' columns) or a result class of SIEVER, DMAR or lavaan. A file that holds an
#' environment (and so any active binding or unevaluated promise in it), a
#' function, a promise, R code, an external pointer, an S4 object or another
#' class is refused with the place and the type or class found, and nothing
#' in the file is run. The digest in the file's header detects damage, not
#' tampering: anyone can write a file with a matching digest, so the check
#' of the content, not the digest, is what keeps code in a file from
#' running. Open project files from people you trust, as with any file of
#' data.
#'
#' @param file The path of a project file.
#' @return A checked snapshot at the current schema, with the attribute
#'   `container` (the container version the file was written in, 0 for a
#'   plain RDS).
#' @author Ken Kelley
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' project <- sieve_project_read(path)
#' project$meta[, c("item", "construct", "type", "status")]
#' project$confirmed
#' @export
sieve_project_read <- function(file) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    sieve_project_stop("The project file was not found. Choose a file that SIEVER's Save project wrote.")
  }
  bytes <- readBin(file, "raw", file.size(file))
  found <- sieve_project_decode(bytes)
  if (is.null(found)) {
    # A file that is not a container: a plain RDS of the snapshot, with no
    # digest of its own. The checks a snapshot passes still apply, the check
    # for plain data first, on the object held in a list.
    read <- sieve_capture(list(readRDS(file)))
    if (!read$ok) {
      sieve_project_stop(
        paste(
          "The file could not be read as a SIEVER project (%s). It is damaged or it is another",
          "kind of file. Choose a file that SIEVER's Save project wrote."
        ),
        read$error
      )
    }
    sieve_project_plain(read$value)
    snapshot <- .subset2(read$value, 1L)
    container <- 0L
  } else {
    snapshot <- found$snapshot
    container <- found$container
  }
  if (!is.list(snapshot) || !identical(snapshot$format, "SIEVER project")) {
    sieve_project_stop(
      "The file is not a SIEVER project (it is some other R data file). Choose a file that SIEVER's Save project wrote."
    )
  }
  # Only now, with the content known to be plain data (sieve_project_plain()
  # above), are its fields read and its labelled columns prepared.
  snapshot <- sieve_labelled_ready(snapshot)
  bad <- sieve_not_plain(unclass(snapshot))
  if (length(bad)) {
    sieve_project_stop(
      "The file holds something that is not plain data (%s), so it was not opened. Project files hold only data; nothing in the file was run.",
      paste(utils::head(bad, 3L), collapse = "; ")
    )
  }
  snapshot <- sieve_project_migrate(snapshot)
  problems <- sieve_project_check(snapshot)
  if (length(problems)) {
    sieve_project_stop("The project was not opened: %s.", paste(problems, collapse = "; "))
  }
  attr(snapshot, "container") <- container
  # The content the checks passed on, so a restore of this very snapshot
  # does not run them a second time; a snapshot
  # changed after it was read does not carry a matching mark.
  attr(snapshot, "sieve_checked") <- sieve_snapshot_digest(snapshot)
  snapshot
}

#' The content key of a snapshot, without its attributes
#'
#' @param snapshot A snapshot.
#' @return A hash string.
#' @author Ken Kelley
#' @noRd
sieve_snapshot_digest <- function(snapshot) {
  plain <- unclass(snapshot)
  attributes(plain) <- list(names = names(plain))
  sieve_hash(plain)
}

#' What a project file holds, in words
#'
#' For the Open project dialog, which shows this before the researcher
#' opens the file, and for the notification afterwards.
#'
#' @param snapshot A checked snapshot from [sieve_project_read()].
#' @return A named character vector: `dataset`, `rows`, `items`, `roles`,
#'   `constructs`, `reverse_scored`, `confirmed`, `estimator_setting`,
#'   `records`, `analyses`,
#'   `steps`, `saved`, `siever`, `schema`, and `file_form`.
#' @author Ken Kelley
#' @noRd
sieve_project_describe <- function(snapshot) {
  meta <- snapshot$meta
  roles <- snapshot$roles
  constructs <- unique(as.character(meta$construct)[sieve_construct_assigned(meta$construct)])
  container <- attr(snapshot, "container")
  migrated <- snapshot$migrated_from
  c(
    dataset = as.character(snapshot$dataset$name %||% "(no name)"),
    # The rows in the analysis, with the rows as loaded and the respondents
    # removed on the Screening tab.
    rows = {
      held <- snapshot$other$removed_cases %||% list()
      removed <- length(held)
      # Rows reserved for validation are removed through the same path and
      # carry the reservation as their marker (fct_holdout.R).
      reserved <- sum(vapply(held, function(r) is.list(r) && !is.null(r$holdout), logical(1)))
      n <- nrow(snapshot$raw)
      if (removed) {
        sprintf(
          "%s in the analysis (%s loaded, %s removed on the Screening tab%s)",
          format(n, big.mark = ","), format(n + removed, big.mark = ","), format(removed, big.mark = ","),
          if (reserved) sprintf(", %s of them reserved for validation", format(reserved, big.mark = ",")) else ""
        )
      } else {
        sprintf("%s (none removed)", format(n, big.mark = ","))
      }
    },
    items = sprintf(
      "%d (%d included, %d excluded)", nrow(meta), sum(meta$status == "active"),
      sum(meta$status != "active")
    ),
    roles = sieve_roles_summary(roles),
    constructs = if (length(constructs)) {
      sprintf("%d (%s)", length(constructs), paste(constructs, collapse = ", "))
    } else {
      "none assigned"
    },
    reverse_scored = as.character(sum(meta$reverse %in% TRUE)),
    confirmed = if (isTRUE(snapshot$confirmed)) {
      sprintf("yes (%s)", snapshot$estimator)
    } else {
      "no (the item types await confirmation)"
    },
    estimator_setting = if (sieve_snapshot_legacy_auto(snapshot)) {
      paste(
        "Follow the item types, with continuous items recorded as fit under MLR with FIML; opens as",
        sieve_estimator_choice_text("mlr"), "(see the note on the Ingest tab)"
      )
    } else {
      sieve_estimator_choice_text(sieve_snapshot_choice(snapshot))
    },
    records = as.character(length(snapshot$transforms)),
    analyses = as.character(length(snapshot$analyses)),
    steps = as.character(sum(startsWith(snapshot$codelog, "# ["))),
    saved = sprintf("%s (R %s)", snapshot$saved$time %||% "unknown", snapshot$saved$r %||% "?"),
    siever = as.character(snapshot$siever_version %||% "unknown"),
    schema = sprintf(
      "%d%s", as.integer(snapshot$schema_version),
      if (length(migrated)) sprintf(" (upgraded from %d)", migrated[1]) else ""
    ),
    file_form = if (is.null(container) || identical(container, 0L)) {
      "plain RDS, without a digest of its own"
    } else {
      sprintf("project file version %d, digest verified", container)
    }
  )
}

#' Put a project snapshot into the session
#'
#' The snapshot is brought to the current schema and checked before
#' anything is touched; when it does not pass, the function stops and the
#' session is exactly as it was. When it passes, every field is written in
#' one step: the data as loaded, the item data, the ledger, the theory map,
#' the roles, the confirmation, the estimator, the study's estimator
#' setting (`"auto"` for a project that holds none; `"mlr"`, logged, for a
#' study that records MLR with FIML under `"auto"`, see
#' `sieve_keep_legacy_mlr()`), the thresholds, the
#' transformation records, the analysis registry, the code log and the
#' ledger rows held aside are the saved ones, replacing those of the
#' session; every other field of the session is set from the file when the
#' file has it and otherwise to the value a new session starts with, so
#' nothing of the session that opened the file outlives the opening. The
#' data then get a new dataset identity, one that neither the session nor
#' the saved records have used, so no undo record, search candidate or
#' cached result from before the restore applies to the restored data; the
#' saved records that belong to the saved dataset are given the new
#' identity, because the restored data are that dataset, so a saved
#' analysis is current when the project is reopened.
#'
#' @param state The shared state store.
#' @param snapshot A snapshot (from [sieve_project_read()] or
#'   `sieve_project_snapshot()`).
#' @param source Optional: the name of the file the snapshot came from,
#'   kept in `state$project`.
#' @return Invisibly, a list with `dataset_id` (the new identity),
#'   `saved_dataset_id` (the identity the data had when they were saved),
#'   and `revision` (the revision after the restore).
#' @author Ken Kelley
#' @noRd
sieve_project_restore <- function(state, snapshot, source = NULL) {
  container <- attr(snapshot, "container")
  checked <- attr(snapshot, "sieve_checked")
  snapshot <- sieve_project_migrate(snapshot)
  # A snapshot that sieve_project_read() checked and that is unchanged since
  # (its content key is the one the read recorded) is not checked again.
  if (!(is.character(checked) && identical(checked, sieve_snapshot_digest(snapshot)))) {
    problems <- sieve_project_check(snapshot)
    if (length(problems)) {
      sieve_project_stop("The project was not opened: %s.", paste(problems, collapse = "; "))
    }
  }
  shiny::isolate({
    # The highest dataset number either history has used, so the identity
    # given below is new to both.
    used <- c(
      snapshot$dataset$id,
      unlist(lapply(snapshot$transforms, function(r) c(r$dataset_id, r$revision$dataset_id))),
      unlist(lapply(snapshot$analyses, function(r) r$revision$dataset_id))
    )
    used <- suppressWarnings(as.integer(sub("^D", "", grep("^D[0-9]+$", as.character(used), value = TRUE))))
    highest <- max(c(0L, state$dataset_seq %||% 0L, snapshot$dataset$seq %||% 0L, used), na.rm = TRUE)

    state$dataset_name <- snapshot$dataset$name
    state$raw <- snapshot$raw
    state$items <- snapshot$items
    sieve_write_ledger(state, snapshot$meta)
    state$theory <- snapshot$theory
    state$roles <- snapshot$roles
    state$confirmed <- snapshot$confirmed
    state$estimator <- snapshot$estimator
    # The study's estimator setting; a project that holds none follows the
    # item types.
    state$estimator_choice <- sieve_snapshot_choice(snapshot)
    state$construct_basis <- snapshot$construct_basis %||% "theoretical"
    state$thresholds <- snapshot$thresholds %||% sieve_default_thresholds()
    state$palette <- snapshot$palette %||% "nd"
    state$codelog <- snapshot$codelog
    state$held_rows <- snapshot$held_rows %||% list()
    # Every other field of the session: from the file when the file has it,
    # otherwise the value a new session starts with. A file never adds a
    # field to the state, and the session that opened the file leaves
    # nothing of its own behind.
    fresh <- shiny::reactiveValuesToList(sieve_new_state())
    known <- setdiff(names(shiny::reactiveValuesToList(state)), sieve_project_fields())
    preferences <- names(Filter(function(field) !isTRUE(field$reset), sieve_state_fields()))
    known <- setdiff(known, preferences)
    for (field in known) {
      state[[field]] <- if (field %in% names(snapshot$other)) snapshot$other[[field]] else fresh[[field]]
    }
    # Page badges describe calculations in this session, never saved evidence.
    state$page_results <- list()
    state$dataset_seq <- as.integer(highest)
    sieve_new_dataset(state)
    # The respondents removed from the analysis are the saved ones (kept with
    # the additional fields); a new dataset identity clears them, so they are
    # put back after it.
    state$case_index <- snapshot$other$case_index
    state$removed_cases <- snapshot$other$removed_cases %||% list()
    # Scientific identity survives reopening; the new load identity above
    # invalidates session caches. Original decision revisions and keys stay intact.
    state$scientific_dataset_id <- snapshot$other$scientific_dataset_id %||%
      paste0("sample_", sieve_hash(list(snapshot$dataset$id, snapshot$keys$raw)))
    # The saved records of the saved dataset belong to the data now loaded.
    from <- snapshot$dataset$id
    to <- state$dataset_id
    relabel <- function(record) {
      if (!is.list(record) || is.na(from) || !nzchar(from)) {
        return(record)
      }
      if (identical(record$dataset_id, from)) {
        record$dataset_id <- to
      }
      for (part in c("revision", "revision_before")) {
        if (is.list(record[[part]]) && identical(record[[part]]$dataset_id, from)) {
          record[[part]]$dataset_id <- to
        }
      }
      record
    }
    state$transforms <- lapply(snapshot$transforms, relabel)
    state$analyses <- lapply(snapshot$analyses, relabel)
    # The validation reservations belong to the data now loaded (fct_holdout.R).
    state$holdout <- lapply(snapshot$other$holdout %||% list(), relabel)
    state$decisions <- lapply(snapshot$other$decisions %||% list(), function(record) {
      bound <- record$load_dataset_id %||% record$revision$dataset_id
      if (identical(bound, from)) {
        record$load_dataset_id <- to
        record$scientific_dataset_id <- state$scientific_dataset_id
      }
      record
    })
    if (!length(state$replay_states %||% list())) {
      state$replay_baseline <- sieve_replay_store(state, sieve_replay_inputs(state))
    }
    if (isTRUE(state$confirmed)) {
      confirmation <- state$confirmation
      if (is.null(confirmation)) {
        sieve_remember_confirmation(state, "project", record_time = FALSE)
      } else {
        confirmation$dataset_id <- state$dataset_id
        state$confirmation <- confirmation
      }
    }
    state$project <- list(
      source = source,
      schema_version = snapshot$schema_version,
      migrated_from = snapshot$migrated_from,
      container_version = container,
      siever_version = snapshot$siever_version,
      saved = snapshot$saved,
      saved_dataset_id = snapshot$dataset$id,
      dataset_id = state$dataset_id
    )
    # A study that records MLR with FIML under Follow the item types keeps
    # MLR with FIML, the estimator it was fit with, recorded and logged
    # (see sieve_keep_legacy_mlr()).
    sieve_keep_legacy_mlr(state, snapshot)
    invisible(list(
      dataset_id = state$dataset_id,
      saved_dataset_id = snapshot$dataset$id,
      revision = sieve_revision(state)
    ))
  })
}

#' Open a project in the session, with the five-option rule
#'
#' What both Open routes (the Ingest tab and the project bar) do: the
#' snapshot is restored with `sieve_project_restore()`, and then, with the
#' five-option rule on (the checkbox in step 1 of the Ingest tab,
#' `min_options`), every item typed ordered with five or more response
#' options whose type is proposed (`type_source`, see
#' `sieve_type_source()`; a study that records none reads its types as
#' proposed) is read as continuous, and a type
#' the researcher chose is kept.
#' Nothing switches silently: the switch is logged, a project that was
#' saved confirmed is confirmed again with the switched types (also
#' logged), and `state$project$switched_types` holds what the Ingest tab
#' shows on its status chip and above the item grid, with the one click
#' that keeps the items ordered (`sieve_keep_ordered()`). A stored scoring
#' policy repaired on reading is kept in `state$project$score_policy_issues`
#' (see `sieve_score_policy_issues()`), which the Ingest tab shows as well.
#'
#' @param state The shared state store.
#' @param snapshot A snapshot from [sieve_project_read()].
#' @param source The file name, kept in `state$project` (and the project's
#'   name when it has none).
#' @param min_options The fewest response options read as continuous, or
#'   `NULL` with the rule off (the project's types are kept as saved).
#' @return Invisibly, the `sieve_project_restore()` result with `rule` (the
#'   `sieve_apply_option_rule()` result) and `acknowledged` (the project
#'   fingerprint of the session as the file holds it, before the rule).
#' @author Ken Kelley
#' @noRd
sieve_project_open <- function(state, snapshot, source = NULL, min_options = 5L) {
  out <- sieve_project_restore(state, snapshot, source = source)
  # An untitled project takes the name of its file.
  if (!is.null(source) && identical(shiny::isolate(state$project_name), "Untitled project")) {
    state$project_name <- sub("[.]siever[.]rds$", "", source, ignore.case = TRUE)
  }
  # The session as the file holds it, before the rule: the saved copy a
  # researcher has, so a switch shows as a change from it.
  out$acknowledged <- sieve_project_fingerprint(state)
  rule <- sieve_apply_option_rule(state, min_options = min_options, source = "the project")
  shiny::isolate({
    again <- FALSE
    if (length(rule$switched) && isTRUE(rule$was_confirmed)) {
      sieve_confirm_types(state, origin = "rule")
      again <- TRUE
    }
    project <- state$project %||% list()
    project$switched_types <- if (length(rule$switched)) {
      list(
        items = rule$switched, options = rule$options[rule$switched],
        was_confirmed = isTRUE(rule$was_confirmed), confirmed_again = again,
        text = sieve_switched_text(rule$switched, confirmed_again = again)
      )
    }
    project$kept_types <- rule$kept
    # Stored scoring policies repaired on reading (an unknown method, as
    # from a later version of SIEVER, or an invalid minimum): the project
    # opens, and the Ingest tab says what is in force instead.
    project$score_policy_issues <- sieve_score_policy_issues(state$scoring_policies %||% list())
    state$project <- project
  })
  out$rule <- rule
  invisible(out)
}

#' The switch the five-option rule made when a project was opened, in words
#'
#' @param switched The items read as continuous.
#' @param confirmed_again Whether the study was confirmed again with them.
#' @return One paragraph, or `NULL` when nothing was switched.
#' @author Ken Kelley
#' @noRd
sieve_switched_text <- function(switched, confirmed_again = FALSE) {
  if (!length(switched)) {
    return(NULL)
  }
  paste(
    sieve_option_rule_text(switched, "the project"),
    if (isTRUE(confirmed_again)) {
      "The project was saved confirmed, so the study was confirmed again with these items as continuous (the estimator follows the types)."
    } else {
      "The project was saved unconfirmed; confirm the study in step 4 when the types are as you want them."
    },
    "Keep them ordered to return them to the types the project was saved with (recorded as your choice)."
  )
}

#' Keep items the five-option rule switched on opening as ordered
#'
#' The one click the Ingest tab offers with the note: the items are typed
#' ordered again and recorded as the researcher's choice
#' (`type_source = "chosen"`, through `sieve_set_types()`), so the rule
#' keeps them the next time the project is opened; a study that was
#' confirmed with them switched is confirmed again with them ordered, the
#' types it was saved with. The note is cleared.
#'
#' @param state The shared state store.
#' @return Invisibly, the `sieve_set_types()` result.
#' @author Ken Kelley
#' @noRd
sieve_keep_ordered <- function(state) {
  shiny::isolate({
    project <- state$project %||% list()
    held <- project$switched_types
    if (is.null(held) || !length(held$items)) {
      return(invisible(list(changed = character(0), text = "No item was switched when the project was opened.")))
    }
    out <- sieve_set_types(state, held$items, "ordered",
      what = "the items the five-option rule switched when the project was opened"
    )
    if (isTRUE(held$confirmed_again)) {
      sieve_confirm_types(state, origin = "button")
      out$text <- sprintf(
        "%s typed ordered again, as the project was saved, and recorded as your choice; the study is confirmed again with them.",
        sieve_name_some(held$items)
      )
    }
    project$switched_types <- NULL
    state$project <- project
    invisible(out)
  })
}

# ---- Declared groups ----------------------------------------------------------
