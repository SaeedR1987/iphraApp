/* Sidebar for assessment-step navigation. Drives the hidden #workflow_nav tabset. */
(function () {
  var COLLAPSED_KEY = "phr_sidebar_collapsed";

  var statuses = {};
  var STATUS_TITLES = { red: "Not started", yellow: "In progress", green: "Complete", grey: "No status yet" };

  function applyStatus() {
    $(".phr-sidebar__link[data-phr-tab]").each(function () {
      var v = $(this).attr("data-phr-tab");
      var s = statuses[v];
      if (!s) { return; }
      var dot = $(this).children(".phr-status-dot");
      if (!dot.length) {
        dot = $('<span class="phr-status-dot" aria-hidden="true"></span>').prependTo(this);
      }
      dot.attr("class", "phr-status-dot phr-status-dot--" + s).attr("title", STATUS_TITLES[s] || "");
    });
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("phr_step_status", function (msg) {
      statuses = msg || {};
      applyStatus();
    });
  });

  function syncLabels() {
    $(".phr-sidebar__link[data-phr-label-from]").each(function () {
      var src = document.getElementById($(this).attr("data-phr-label-from"));
      if (src) {
        var t = $(src).text();
        $(this).find(".phr-sidebar__label").text(t);
        $(this).attr("title", t);
      }
    });
  }

  function syncActive() {
    var v = $("#workflow_nav li.active a").attr("data-value");
    $(".phr-sidebar__link").each(function () {
      var on = $(this).attr("data-phr-tab") === v;
      $(this).toggleClass("active", on);
      if (on) { $(this).attr("aria-current", "page"); } else { $(this).removeAttr("aria-current"); }
    });
    $(".phr-sidebar__group").each(function () {
      var toggle = $(this).children(".phr-sidebar__group-toggle");
      var gt = toggle.attr("data-phr-group-tab");
      if (gt) { $(this).toggleClass("has-active", gt === v); return; }
      var has = $(this).find(".phr-sidebar__link.active").length > 0;
      $(this).toggleClass("has-active", has);
      if (has) {
        $(this).addClass("open");
        $(this).children(".phr-sidebar__group-toggle").attr("aria-expanded", "true");
      }
    });
  }

  function setCollapsed(c) {
    $("body").toggleClass("phr-sidebar-collapsed", c);
    $("#phr_sidebar_toggle").attr("aria-expanded", c ? "false" : "true");
    try { localStorage.setItem(COLLAPSED_KEY, c ? "1" : "0"); } catch (e) {}
  }

  $(document).on("click", ".phr-sidebar__group-toggle", function () {
    var li = $(this).closest(".phr-sidebar__group");
    var open = !li.hasClass("open");
    li.toggleClass("open", open);
    $(this).attr("aria-expanded", open ? "true" : "false");
    var gt = $(this).attr("data-phr-group-tab");
    if (gt && open) { $('#workflow_nav a[data-value="' + gt + '"]').tab("show"); }
  });
  $(document).on("click", ".phr-sidebar__link[data-phr-tab]", function (e) {
    e.preventDefault();
    $('#workflow_nav a[data-value="' + $(this).attr("data-phr-tab") + '"]').tab("show");
  });
  $(document).on("click", "#phr_sidebar_toggle", function () {
    setCollapsed(!$("body").hasClass("phr-sidebar-collapsed"));
  });
  $(document).on("shown.bs.tab", "#workflow_nav a", syncActive);
  $(document).on("shiny:value shiny:connected", function () {
    setTimeout(syncLabels, 0);
    setTimeout(syncActive, 0);
  });

  $(function () {
    var stored = null;
    try { stored = localStorage.getItem(COLLAPSED_KEY); } catch (e) {}
    if (stored === "1") { setCollapsed(true); }
    syncLabels();
    syncActive();
  });
})();
