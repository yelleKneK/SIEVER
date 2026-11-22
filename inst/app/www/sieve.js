// SIEVER client presentation preferences. Widths are saved in this browser's
// localStorage by table and semantic column key; no participant data are stored.
(function () {
  var memory = {}, selected = {}, states = {}, scrolls = {}, tooltip, tooltipOwner, keyboardExplanations = false;
  var prefix = "siever.presentation.v1:", actionStates = {};
  function saved(id) {
    if (!memory[id]) {
      try { memory[id] = JSON.parse(localStorage.getItem(prefix + id) || "{}"); }
      catch (e) { memory[id] = {}; }
    }
    return memory[id];
  }
  function persist(id) {
    try { localStorage.setItem(prefix + id, JSON.stringify(saved(id))); }
    catch (e) { /* A private browser may only retain this session's preferences. */ }
  }
  function setting(tbl) {
    if (!window.jQuery || !jQuery.fn.dataTable || !jQuery.fn.dataTable.isDataTable(tbl)) return null;
    return jQuery(tbl).DataTable().settings()[0];
  }
  function config(st) { return st && st.oInit.sieve; }
  window.sieveTableStateSave = function (st, data) {
    var cfg = config(st);
    if (cfg) states[cfg.id] = JSON.parse(JSON.stringify(data));
  };
  window.sieveTableStateLoad = function (st) {
    var cfg = config(st);
    return cfg && states[cfg.id] ? JSON.parse(JSON.stringify(states[cfg.id])) : null;
  };
  function tableParts(st) {
    return [st.nTable, st.nScrollHead && st.nScrollHead.querySelector("table")].filter(Boolean);
  }
  function visibleIndices(st) {
    return st.aoColumns.map(function (c, i) { return c.bVisible ? i : -1; }).filter(function (i) { return i >= 0; });
  }
  function applyWidths(st) {
    var cfg = config(st);
    if (!cfg) return;
    var widths = saved(cfg.id), indices = visibleIndices(st);
    tableParts(st).forEach(function (table) {
      var heads = table.querySelectorAll("thead tr:first-child th");
      var cols = table.querySelectorAll("colgroup col");
      var total = 0, fixed = false;
      indices.forEach(function (index, pos) {
        var key = cfg.keys[index], width = widths[key] || (cfg.widths && cfg.widths[key]);
        var th = heads[pos];
        if (!th) return;
        th.setAttribute("data-sieve-key", key);
        th.setAttribute("data-sieve-index", index);
        if (width) {
          fixed = true;
          [th, cols[pos]].filter(Boolean).forEach(function (cell) {
            cell.style.setProperty("box-sizing", "border-box", "important");
            cell.style.width = width + "px";
            cell.style.minWidth = width + "px";
            cell.style.maxWidth = width + "px";
          });
          st.aoColumns[index].sWidth = width + "px";
        }
        total += width || th.getBoundingClientRect().width;
      });
      if (fixed && total > 0) table.style.width = total + "px";
    });
    // Apply by original column index. Hidden columns never borrow another key.
    indices.forEach(function (index) {
      var width = widths[cfg.keys[index]] || (cfg.widths && cfg.widths[cfg.keys[index]]);
      if (!width) return;
      new jQuery.fn.dataTable.Api(st).column(index).nodes().each(function (td) {
        td.style.width = width + "px";
        td.style.minWidth = width + "px";
        td.style.maxWidth = width + "px";
      });
    });
  }
  function explain(st) {
    var cfg = config(st), indices = visibleIndices(st);
    if (!cfg) return;
    tableParts(st).forEach(function (table) {
      table.querySelectorAll("thead tr:first-child th").forEach(function (th, pos) {
        var index = indices[pos], key = cfg.keys[index];
        if (!key) return;
        var label = (th.textContent || "").trim();
        var text = label + (cfg.definitions && cfg.definitions[key] ? ": " + cfg.definitions[key] : "");
        th.setAttribute("data-sieve-explanation", text);
        th.setAttribute("aria-label", text);
        th.setAttribute("scope", "col");
        th.setAttribute("tabindex", "-1");
        th.id = cfg.id + "-" + key + (table === st.nTable ? "-body" : "-head");
      });
    });
    tableParts(st).forEach(function (table) {
      table.querySelectorAll("[title]").forEach(function (cell) {
        var text = cell.getAttribute("title");
        cell.setAttribute("data-sieve-explanation", text);
        if (!cell.getAttribute("aria-label")) cell.setAttribute("aria-label", text);
        cell.removeAttribute("title");
      });
    });
    var api = new jQuery.fn.dataTable.Api(st);
    indices.forEach(function (index) {
      api.column(index).nodes().each(function (td) {
        td.setAttribute("headers", cfg.id + "-" + cfg.keys[index] + (st.nScrollHead ? "-head" : "-body"));
        if (!td.querySelector("input,select,button,[tabindex]")) {
          td.setAttribute("tabindex", "-1");
          if (td.scrollWidth > td.clientWidth) td.setAttribute("data-sieve-explanation", td.textContent.trim());
        }
      });
    });
  }
  function addResizers(st) {
    var cfg = config(st);
    if (!cfg) return;
    tableParts(st).forEach(function (table) {
      table.querySelectorAll("thead th[data-sieve-key]").forEach(function (th) {
        if (th.querySelector(".col-resizer")) return;
        th.style.position = "relative";
        var handle = document.createElement("span");
        handle.className = "col-resizer";
        handle.setAttribute("role", "separator");
        handle.setAttribute("aria-orientation", "vertical");
        handle.setAttribute("aria-label", "Resize " + th.textContent.trim() + "; arrow keys adjust width");
        handle.tabIndex = -1;
        th.appendChild(handle);
        function set(width) {
          saved(cfg.id)[th.getAttribute("data-sieve-key")] = Math.max(48, Math.round(width));
          applyWidths(st);
        }
        handle.addEventListener("click", function (e) { e.stopPropagation(); });
        handle.addEventListener("keydown", function (e) {
          if (e.key !== "ArrowLeft" && e.key !== "ArrowRight") return;
          e.preventDefault(); e.stopPropagation();
          set(th.getBoundingClientRect().width + (e.key === "ArrowRight" ? 10 : -10));
          persist(cfg.id);
        });
        handle.addEventListener("mousedown", function (e) {
          e.preventDefault(); e.stopPropagation();
          var startX = e.pageX, startW = th.getBoundingClientRect().width;
          function move(ev) { set(startW + ev.pageX - startX); }
          function up() {
            document.removeEventListener("mousemove", move);
            document.removeEventListener("mouseup", up);
            persist(cfg.id);
            new jQuery.fn.dataTable.Api(st).columns.adjust().draw(false);
            applyWidths(st);
          }
          document.addEventListener("mousemove", move);
          document.addEventListener("mouseup", up);
        });
      });
    });
  }
  function reportSelection(st) {
    var cfg = config(st);
    if (!cfg || !cfg.selectionInput || !window.Shiny) return;
    var api = new jQuery.fn.dataTable.Api(st), index = cfg.keys.indexOf(cfg.rowKey);
    var ids = [];
    api.rows().every(function () {
      var node = this.node(), row = this.data();
      if (node && node.classList.contains("selected")) ids.push(String(row[index]));
    });
    selected[cfg.id] = ids;
    Shiny.setInputValue(cfg.selectionInput, ids, {priority: "event"});
  }
  function enhance(tbl) {
    var st = setting(tbl);
    if (!st) return;
    explain(st); applyWidths(st); addResizers(st); rovingFocus(st);
    var cfg = config(st);
    if (cfg && st.nScrollBody && !st._sieveScrollBound) {
      st._sieveScrollBound = true;
      var held = scrolls[cfg.id];
      if (held) { st.nScrollBody.scrollLeft = held.left; st.nScrollBody.scrollTop = held.top; }
      st.nScrollBody.addEventListener("scroll", function () {
        scrolls[cfg.id] = {left: st.nScrollBody.scrollLeft, top: st.nScrollBody.scrollTop};
      });
    }
    var api = new jQuery.fn.dataTable.Api(st), pages = api.page.info().pages;
    jQuery(st.nTableWrapper).find(".dataTables_paginate, .dt-paging").toggle(pages > 1);
  }
  // One Tab stop per item grid. Arrow keys inspect cells; Enter/F2 enters
  // an embedded editor or resize control; Escape returns to the cell.
  function focusableCells(st) {
    var api = new jQuery.fn.dataTable.Api(st);
    var rows = Array.from(api.rows({page: "current"}).nodes());
    var heads = Array.from((st.nScrollHead || st.nTHead).querySelectorAll("thead tr:first-child th, tr:first-child th"));
    var grid = heads.length ? [heads] : [];
    rows.forEach(function (row) { grid.push(Array.from(row.children).filter(function (cell) { return cell.offsetParent !== null; })); });
    return grid.filter(function (row) { return row.length; });
  }
  function rovingFocus(st) {
    var cfg = config(st);
    if (!cfg) return;
    var grid = focusableCells(st), cells = [].concat.apply([], grid);
    cells.forEach(function (cell) {
      cell.tabIndex = -1;
      cell.querySelectorAll("input,select,button,.col-resizer").forEach(function (node) { node.tabIndex = -1; });
    });
    var at = st._sieveFocus || {row: grid.length > 1 ? 1 : 0, col: 0};
    if (at.item && cfg.rowKey) {
      var api = new jQuery.fn.dataTable.Api(st), keyIndex = cfg.keys.indexOf(cfg.rowKey);
      var found = grid.findIndex(function (row) {
        if (!row[0] || !row[0].closest("tbody")) return false;
        var data = api.row(row[0].parentElement).data();
        return data && String(data[keyIndex]) === at.item;
      });
      if (found >= 0) at.row = found;
    }
    at.row = Math.min(at.row, Math.max(0, grid.length - 1));
    if (!grid[at.row]) return;
    at.col = Math.min(at.col, grid[at.row].length - 1);
    grid[at.row][at.col].tabIndex = 0;
    st._sieveFocus = at;
    var helpId = cfg.id + "-keyboard-help", help = document.getElementById(helpId);
    if (!help) {
      help = document.createElement("p"); help.id = helpId; help.className = "small text-muted";
      help.textContent = "Grid keyboard: arrow keys inspect cells; Enter or F2 edits; Escape returns to the cell; Tab leaves the grid. Arrow down from the last row continues to the next page. Arrow up from the first row reaches column definitions and resizing.";
      st.nTableWrapper.insertBefore(help, st.nTableWrapper.firstChild);
    }
    st.nTable.setAttribute("aria-describedby", helpId);
    if (st._sieveRefocus) { st._sieveRefocus = false; grid[at.row][at.col].focus(); }
  }
  function navigateCell(e) {
    var cell = e.target.closest("td,th"), tbl = e.target.closest("table");
    if (!cell || !tbl) return;
    var st = setting(tbl);
    if (!st) {
      var wrap = tbl.closest(".dataTables_wrapper,.dt-container");
      if (wrap) { var body = wrap.querySelector(".dataTables_scrollBody table,.dt-scroll-body table"); if (body) st = setting(body); }
    }
    if (!config(st)) return;
    var editor = e.target !== cell && e.target.closest("input,select,button,.col-resizer");
    if (editor) {
      if (e.key === "Escape") { e.preventDefault(); cell.focus(); }
      return;
    }
    if (e.key === "Enter" || e.key === "F2") {
      var control = cell.querySelector("input,select,button,.col-resizer");
      if (control) { e.preventDefault(); e.stopPropagation(); control.focus(); }
      return;
    }
    var directions = {ArrowLeft: [0, -1], ArrowRight: [0, 1], ArrowUp: [-1, 0], ArrowDown: [1, 0]};
    if (!directions[e.key]) return;
    var grid = focusableCells(st), row = grid.findIndex(function (r) { return r.indexOf(cell) >= 0; });
    if (row < 0) return;
    e.preventDefault(); e.stopPropagation();
    var col = grid[row].indexOf(cell), nextRow = row + directions[e.key][0], nextCol = col + directions[e.key][1];
    var api = new jQuery.fn.dataTable.Api(st), page = api.page.info();
    if (nextRow >= grid.length && page.page + 1 < page.pages) {
      st._sieveFocus = {row: 1, col: col}; st._sieveRefocus = true; st._sievePageMove = true; api.page("next").draw("page"); return;
    }
    if (nextRow < 0) nextRow = 0;
    nextRow = Math.min(nextRow, grid.length - 1); nextCol = Math.max(0, Math.min(nextCol, grid[nextRow].length - 1));
    grid.forEach(function (r) { r.forEach(function (c) { c.tabIndex = -1; }); });
    var rowData = api.row(grid[nextRow][nextCol].parentElement).data();
    var cfg = config(st), keyIndex = cfg.keys.indexOf(cfg.rowKey);
    st._sieveFocus = {row: nextRow, col: nextCol, item: rowData && keyIndex >= 0 ? String(rowData[keyIndex]) : null};
    grid[nextRow][nextCol].tabIndex = 0; grid[nextRow][nextCol].focus();
  }
  document.addEventListener("keydown", navigateCell);
  function showExplanation(target) {
    if (!target || !target.getAttribute) return;
    var text = target.getAttribute("data-sieve-explanation");
    if (!text) return;
    if (!tooltip) {
      tooltip = document.createElement("div");
      tooltip.id = "sieve-accessible-explanation";
      tooltip.className = "sieve-accessible-explanation";
      tooltip.setAttribute("role", "tooltip");
      document.body.appendChild(tooltip);
    }
    tooltip.textContent = text;
    tooltip.hidden = false;
    tooltipOwner = target;
    target.setAttribute("aria-describedby", tooltip.id);
    var box = target.getBoundingClientRect();
    tooltip.style.left = Math.max(8, Math.min(box.left, window.innerWidth - 430)) + "px";
    tooltip.style.top = Math.min(box.bottom + 4, window.innerHeight - tooltip.offsetHeight - 8) + "px";
  }
  function hideExplanation() {
    if (tooltip) tooltip.hidden = true;
    if (tooltipOwner) tooltipOwner.removeAttribute("aria-describedby");
    tooltipOwner = null;
  }
  document.addEventListener("keydown", function (e) {
    keyboardExplanations = true;
    if (e.key === "Escape") hideExplanation();
  });
  document.addEventListener("pointerdown", function () { keyboardExplanations = false; });
  document.addEventListener("mousemove", function () { keyboardExplanations = false; });
  if (window.jQuery) {
    jQuery(document).on("stateLoadParams.dt", function (e, st, data) {
      var cfg = config(st);
      if (cfg && typeof cfg.advanced === "boolean" && data.columns) {
        ["redundancy"].forEach(function (key) {
          var index = cfg.keys.indexOf(key);
          if (index >= 0 && data.columns[index]) data.columns[index].visible = cfg.advanced;
        });
      }
    });
    jQuery(document).on("preDraw.dt", function (e, st) {
      if (st._sievePageMove) { st._sievePageMove = false; return; }
      var active = document.activeElement, cell = active && active.closest && active.closest("td,th");
      if (config(st) && cell && st.nTableWrapper.contains(cell)) {
        var grid = focusableCells(st), row = grid.findIndex(function (r) { return r.indexOf(cell) >= 0; });
        if (row >= 0) {
          var api = new jQuery.fn.dataTable.Api(st), cfg = config(st), index = cfg.keys.indexOf(cfg.rowKey), data = api.row(cell.parentElement).data();
          st._sieveFocus = {row: row, col: grid[row].indexOf(cell), item: data && index >= 0 ? String(data[index]) : null};
          st._sieveRefocus = true;
        }
      }
    });
    jQuery(document).on("draw.dt init.dt column-visibility.dt", function (e) { enhance(e.target); });
    jQuery(document).on("click", "table.dataTable tbody tr", function () {
      var tbl = this.closest("table");
      setTimeout(function () { var st = setting(tbl); if (st) reportSelection(st); }, 0);
    });
    jQuery(document).on("change", ".sieve-cell[data-sieve-input]", function () {
      this.setAttribute("data-sieve-explanation", this.getAttribute("data-item") + ": " + this.getAttribute("data-field") + ": " + this.value);
      if (window.Shiny) Shiny.setInputValue(this.getAttribute("data-sieve-input"), {
        item: this.getAttribute("data-item"), field: this.getAttribute("data-field"), value: this.value
      }, {priority: "event"});
    });
    function focusedExplanation() {
      var active = document.activeElement;
      return active && active.closest && active.closest("table.dataTable") ? active.closest("[data-sieve-explanation]") : null;
    }
    jQuery(document).on("focusin", "table.dataTable [data-sieve-explanation]", function () {
      showExplanation(focusedExplanation() || this);
    });
    jQuery(document).on("mouseenter", "table.dataTable [data-sieve-explanation]", function () {
      showExplanation((keyboardExplanations && focusedExplanation()) || this);
    });
    jQuery(document).on("focusout", "table.dataTable [data-sieve-explanation]", hideExplanation);
    jQuery(document).on("mouseleave", "table.dataTable [data-sieve-explanation]", function () {
      var active = keyboardExplanations && focusedExplanation();
      if (active) showExplanation(active); else hideExplanation();
    });
    jQuery(document).on("click", ".sieve-reset-widths", function () {
      var id = this.getAttribute("data-sieve-table");
      memory[id] = {}; persist(id);
      jQuery.fn.dataTable.tables({api: true}).settings().each(function (st) {
        if (config(st) && config(st).id === id) {
          tableParts(st).forEach(function (table) {
            table.style.width = "";
            table.querySelectorAll("th,td,col").forEach(function (cell) {
              cell.style.width = ""; cell.style.minWidth = ""; cell.style.maxWidth = "";
            });
          });
          st.aoColumns.forEach(function (col) { col.sWidth = col.sWidthOrig || null; });
          new jQuery.fn.dataTable.Api(st).columns.adjust().draw(false);
        }
      });
    });
  }
  // All construct/facet comboboxes share suggestions, including paginated rows.
  function sieveRefreshDatalists() {
    var groups = {};
    document.querySelectorAll("input.sieve-cell[list]").forEach(function (inp) {
      var id = inp.getAttribute("list"), v = (inp.value || "").trim();
      if (!groups[id]) groups[id] = {};
      if (v) groups[id][v] = true;
    });
    document.querySelectorAll(".sieve-dl-seed").forEach(function (seed) {
      var id = seed.getAttribute("data-dl");
      if (!id) return;
      if (!groups[id]) groups[id] = {};
      (seed.getAttribute("data-vals") || "").split("|").forEach(function (v) {
        v = v.trim(); if (v) groups[id][v] = true;
      });
    });
    Object.keys(groups).forEach(function (id) {
      var dl = document.getElementById(id);
      if (!dl) { dl = document.createElement("datalist"); dl.id = id; document.body.appendChild(dl); }
      dl.replaceChildren();
      Object.keys(groups[id]).sort().forEach(function (value) {
        var opt = document.createElement("option"); opt.value = value; dl.appendChild(opt);
      });
    });
  }
  if (window.jQuery) {
    jQuery(document).on("draw.dt init.dt", sieveRefreshDatalists);
    jQuery(document).on("change", "input.sieve-cell[list]", sieveRefreshDatalists);
    jQuery(document).on("shiny:connected", function () {
      // A modal may arrive after its server-side prerequisite message.
      // Reapply the last state to each newly inserted action control.
      if (document.body && !window.sieveActionObserver) {
        window.sieveActionObserver = new MutationObserver(function () {
          Object.keys(actionStates).forEach(function (id) {
            var node = document.getElementById(id);
            if (node) { node.disabled = actionStates[id]; node.setAttribute("aria-disabled", String(actionStates[id])); }
          });
        });
        window.sieveActionObserver.observe(document.body, {childList: true, subtree: true});
      }
      // Shiny writes "Upload complete" in a file input's progress bar once
      // the browser has handed the file to the R session. Run locally,
      // nothing leaves the computer, so the bar says what the file is ready
      // for instead: the input's data-sieve-done text, or "File ready".
      if (document.body && !window.sieveFileDoneObserver) {
        window.sieveFileDoneObserver = new MutationObserver(function (mutations) {
          mutations.forEach(function (m) {
            var node = m.target.nodeType === 1 ? m.target : m.target.parentElement;
            var bar = node && node.closest && node.closest(".shiny-file-input-progress .progress-bar");
            if (!bar || bar.textContent !== "Upload complete") return;
            var box = bar.closest(".shiny-input-container");
            bar.textContent = (box && box.getAttribute("data-sieve-done")) || "File ready";
          });
        });
        window.sieveFileDoneObserver.observe(document.body, {childList: true, characterData: true, subtree: true});
      }
      // The text under a file box follows the study (what Load would do with
      // the chosen files, or that they are the ones loaded): kept as the
      // box's data-sieve-done, and shown at once when the bar is in view.
      if (window.Shiny) Shiny.addCustomMessageHandler("sieve-file-bar", function (msg) {
        var input = document.getElementById(msg.id);
        var box = input && input.closest(".shiny-input-container");
        if (box) box.setAttribute("data-sieve-done", msg.text);
        var bar = document.querySelector("#" + msg.id + "_progress .progress-bar");
        // A failed transfer keeps Shiny's error text (progress-bar-danger).
        if (bar && bar.textContent && bar.textContent !== "Upload complete" &&
            !bar.classList.contains("progress-bar-danger")) bar.textContent = msg.text;
      });
      // A button whose role changes with the study (Load once a study is in
      // the session) takes the classes the server names.
      if (window.Shiny) Shiny.addCustomMessageHandler("sieve-button-class", function (msg) {
        var node = document.getElementById(msg.id);
        if (!node) return;
        [].concat(msg.remove || []).forEach(function (c) { node.classList.remove(c); });
        [].concat(msg.add || []).forEach(function (c) { node.classList.add(c); });
      });
      if (window.Shiny) Shiny.addCustomMessageHandler("sieve-action-state", function (action) {
        actionStates[action.id] = !!action.disabled;
        var node = document.getElementById(action.id);
        if (node) { node.disabled = !!action.disabled; node.setAttribute("aria-disabled", String(!!action.disabled)); }
      });
      if (window.Shiny) Shiny.addCustomMessageHandler("sieve-workflow-policy", function (policy) {
        document.body.classList.toggle("sieve-empirical", !!policy.optimization);
      });
      // Download this analysis (a code disclosure): the server registers
      // the bundle for the panel's recorded step and names its URL here.
      // A download anchor keeps the application page in place; a response
      // that is not the bundle arrives as a file, never as a page change.
      if (window.Shiny) Shiny.addCustomMessageHandler("sieve-code-download", function (message) {
        if (!message || !message.href) return;
        var link = document.createElement("a");
        link.href = message.href;
        link.setAttribute("download", "");
        link.style.display = "none";
        document.body.appendChild(link);
        link.click();
        setTimeout(function () { link.remove(); }, 0);
      });
    });
  }
  // Code disclosures: copying the recorded code is done in the browser
  // alone (nothing is sent to the server, no analysis runs); the download
  // names the panel's recorded step for the server to serve.
  function sieveCodeCopy(button) {
    var panel = button.closest(".siever-code-view");
    var code = panel && panel.querySelector("pre.sieve-code-source code");
    var note = panel && panel.querySelector(".sieve-code-copy-note");
    var text = code ? code.textContent : "";
    function say(message) { if (note) note.textContent = message; }
    function fallback() {
      var done = false;
      try {
        var range = document.createRange();
        range.selectNodeContents(code);
        var selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
        done = document.execCommand("copy");
      } catch (e) { done = false; }
      say(done ? "Copied." : "This browser did not allow the copy: the code is selected; copy it with the keyboard (Ctrl+C or Cmd+C).");
    }
    if (!text) { say("There is no recorded code to copy."); return; }
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(function () { say("Copied."); }, fallback);
    } else {
      fallback();
    }
  }
  document.addEventListener("click", function (e) {
    var copy = e.target.closest && e.target.closest(".sieve-code-copy");
    if (copy) { e.preventDefault(); sieveCodeCopy(copy); return; }
    var download = e.target.closest && e.target.closest(".sieve-code-download");
    if (download && window.Shiny) {
      e.preventDefault();
      Shiny.setInputValue(download.getAttribute("data-sieve-input"), {
        step: download.getAttribute("data-sieve-step"), nonce: Date.now()
      }, {priority: "event"});
    }
  });
})();

// Focus returns after a dialog closes (WCAG 2.4.3). A Shiny dialog is shown
// from the server, so Bootstrap has no trigger element to return focus to and
// leaves it on the page body. The control that had focus when the dialog
// opened (the button that asked for it) gets it back, if it is still on the
// page and can take it. The handlers are jQuery's: Bootstrap 5 triggers its
// namespaced events through jQuery first when jQuery is present, and Shiny's
// own hidden handler removes the dialog, so the native hidden event that
// follows is dispatched on a detached element and never reaches the document
// (checked in Chrome with Bootstrap 5.3.8: native show 1, native hidden 0,
// jQuery hidden 1).
(function () {
  if (!window.jQuery) return;
  var opener = null;
  jQuery(document).on("show.bs.modal", function () {
    var active = document.activeElement;
    opener = active && active !== document.body ? active : null;
  });
  jQuery(document).on("hidden.bs.modal", function () {
    var target = opener;
    opener = null;
    if (target && document.contains(target) && !target.disabled && typeof target.focus === "function") {
      target.focus();
    }
  });
})();

// Closed settings name the values in force, including server-side updates.
(function () {
  function summaries() {
    document.querySelectorAll("[data-sieve-estimator-override]").forEach(function (el) {
      var box = el.closest("details");
      if (!box) return;
      var choice = el.getAttribute("data-sieve-estimator-override");
      if (choice && choice !== box.dataset.sieveLastOverride) box.open = true;
      box.dataset.sieveLastOverride = choice;
    });
    document.querySelectorAll("[data-sieve-settings]").forEach(function (box) {
      var words = box.getAttribute("data-sieve-settings").split(/\s+/).map(function (id) {
        var el = document.getElementById(id);
        if (!el) return "";
        if (el.type === "checkbox") {
          var label = box.querySelector("label[for='" + id + "']") || el.closest("label");
          return (label ? label.textContent.trim() + ": " : "") + (el.checked ? "yes" : "no");
        }
        if (el.tagName === "SELECT") {
          return Array.from(el.selectedOptions).map(function (x) { return x.textContent; }).join(", ");
        }
        var radios = box.querySelectorAll("input[type=radio][name='" + id + "']:checked");
        if (radios.length) return radios[0].parentElement.textContent.trim();
        var inputLabel = box.querySelector("label[for='" + id + "']");
        return el.value ? (inputLabel ? inputLabel.textContent.trim() + ": " : "") + el.value : "";
      }).filter(Boolean).join("; ");
      var span = box.querySelector(".sieve-setting-values");
      var text = words ? ": " + words : "";
      if (span && span.textContent !== text) span.textContent = text;
    });
  }
  var queued = false;
  function queue() {
    if (queued) return;
    queued = true;
    window.requestAnimationFrame(function () { queued = false; summaries(); });
  }
  document.addEventListener("change", queue);
  document.addEventListener("shiny:value", queue);
  document.addEventListener("DOMContentLoaded", function () {
    new MutationObserver(queue).observe(document.body, {childList: true, subtree: true});
    queue();
  });
})();

// The notification panel, made accessible. Shiny 1.14 builds the panel
// without a live region and closes a notification with a <div> that is not
// focusable, so a notification would be neither announced to a screen
// reader nor dismissible from the keyboard. The panel is given role "status" and aria-live
// "polite" (an error is announced assertively), and each close control is
// made a focusable button labelled Dismiss that Enter, Space and Escape
// operate; Escape anywhere in a notification dismisses it.
function sieveNotificationAccess(root) {
  var panel = document.getElementById("shiny-notification-panel");
  if (panel) {
    if (panel.getAttribute("role") !== "status") panel.setAttribute("role", "status");
    var urgent = panel.querySelector(".shiny-notification-error") ? "assertive" : "polite";
    if (panel.getAttribute("aria-live") !== urgent) panel.setAttribute("aria-live", urgent);
  }
  (root || document).querySelectorAll(".shiny-notification-close").forEach(function (close) {
    if (close.getAttribute("data-sieve-access") === "1") return;
    close.setAttribute("data-sieve-access", "1");
    close.setAttribute("tabindex", "0");
    close.setAttribute("role", "button");
    close.setAttribute("aria-label", "Dismiss");
    close.addEventListener("keydown", function (event) {
      if (event.key === "Enter" || event.key === " " || event.key === "Spacebar" || event.key === "Escape") {
        event.preventDefault();
        close.click();
      }
    });
  });
}
(function () {
  document.addEventListener("keydown", function (event) {
    if (event.key !== "Escape") return;
    var note = event.target && event.target.closest ? event.target.closest(".shiny-notification") : null;
    if (!note) return;
    var close = note.querySelector(".shiny-notification-close");
    if (close) close.click();
  });
  document.addEventListener("DOMContentLoaded", function () {
    new MutationObserver(function () { sieveNotificationAccess(document); })
      .observe(document.body, {childList: true, subtree: true});
    sieveNotificationAccess(document);
  });
})();

// A provenance disclosure keeps the state the researcher gave it for the
// rest of the browser session: a <details> carrying data-sieve-remember
// is opened or closed as it was last left, whatever re-renders it.
(function () {
  var key = function (el) { return "sieve-disclosure:" + el.getAttribute("data-sieve-remember"); };
  document.addEventListener("toggle", function (event) {
    var el = event.target;
    if (!el || !el.getAttribute || !el.getAttribute("data-sieve-remember")) return;
    try { window.sessionStorage.setItem(key(el), el.open ? "open" : "closed"); } catch (e) {}
  }, true);
  function restore() {
    document.querySelectorAll("details[data-sieve-remember]:not([data-sieve-restored])").forEach(function (el) {
      el.setAttribute("data-sieve-restored", "1");
      var saved = null;
      try { saved = window.sessionStorage.getItem(key(el)); } catch (e) {}
      if (saved === "open") el.open = true;
      if (saved === "closed") el.open = false;
    });
  }
  document.addEventListener("DOMContentLoaded", function () {
    new MutationObserver(restore).observe(document.body, {childList: true, subtree: true});
    restore();
  });
})();
