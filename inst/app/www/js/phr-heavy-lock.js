// Heavy-process buttons (.iphra-heavy): once one is clicked, disable them all
// until the server is idle again, so extra clicks cannot queue up.
(function () {
  var SELECTOR = ".iphra-heavy";

  function setLocked(locked) {
    document.querySelectorAll(SELECTOR).forEach(function (el) {
      el.classList.toggle("disabled", locked);
      if (locked) el.setAttribute("disabled", "disabled");
      else el.removeAttribute("disabled");
    });
  }

  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest(SELECTOR);
    if (!btn) return;
    if (btn.hasAttribute("disabled")) {
      e.preventDefault();
      e.stopImmediatePropagation();
      return;
    }
    // Delay so Shiny still registers the click that started the job.
    setTimeout(function () { setLocked(true); }, 0);
  }, true);

  $(document).on("shiny:idle shiny:disconnected", function () { setLocked(false); });
})();
