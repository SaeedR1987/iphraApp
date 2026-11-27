/* Click the framework diagram to export it as SVG, PNG or JPEG. */
(function () {
  var MENU_ID = "phr_svg_export_menu";

  function closeMenu() { $("#" + MENU_ID).remove(); }

  function naturalSize(svg) {
    var vb = svg.viewBox && svg.viewBox.baseVal;
    if (vb && vb.width && vb.height) { return { w: vb.width, h: vb.height }; }
    var w = parseFloat(svg.getAttribute("width")), h = parseFloat(svg.getAttribute("height"));
    if (w && h) { return { w: w, h: h }; }
    var r = svg.getBoundingClientRect();
    return { w: r.width, h: r.height };
  }

  function serialize(svg) {
    var clone = svg.cloneNode(true), size = naturalSize(svg);
    clone.setAttribute("xmlns", "http://www.w3.org/2000/svg");
    clone.setAttribute("xmlns:xlink", "http://www.w3.org/1999/xlink");
    clone.setAttribute("width", size.w);
    clone.setAttribute("height", size.h);
    return { text: new XMLSerializer().serializeToString(clone), size: size };
  }

  function save(blob, name) {
    var a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function () { URL.revokeObjectURL(a.href); a.remove(); }, 500);
  }

  function exportAs(svg, fmt) {
    var s = serialize(svg), stamp = new Date().toISOString().slice(0, 10).replace(/-/g, "");
    var base = "framework_" + stamp;
    if (fmt === "svg") {
      save(new Blob([s.text], { type: "image/svg+xml;charset=utf-8" }), base + ".svg");
      return;
    }
    var scale = 2, img = new Image();
    img.onload = function () {
      var canvas = document.createElement("canvas");
      canvas.width = Math.round(s.size.w * scale);
      canvas.height = Math.round(s.size.h * scale);
      var ctx = canvas.getContext("2d");
      ctx.fillStyle = "#ffffff";
      ctx.fillRect(0, 0, canvas.width, canvas.height);
      ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
      var mime = fmt === "jpeg" ? "image/jpeg" : "image/png";
      canvas.toBlob(function (b) { save(b, base + "." + (fmt === "jpeg" ? "jpg" : "png")); }, mime, 0.95);
    };
    img.onerror = function () { alert("Could not convert the diagram to " + fmt.toUpperCase() + "."); };
    img.src = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(s.text);
  }

  $(document).on("click", "[id$='framework_container'] svg", function (e) {
    e.stopPropagation();
    closeMenu();
    var svg = this;
    var menu = $('<div id="' + MENU_ID + '" style="position:fixed; z-index:20000; background:#fff; ' +
      'border:1px solid #ccc; border-radius:4px; box-shadow:0 2px 8px rgba(0,0,0,.25); padding:6px;"></div>')
      .css({ left: Math.min(e.clientX, window.innerWidth - 170), top: Math.min(e.clientY, window.innerHeight - 140) });
    menu.append('<div style="font-size:11px; font-weight:600; margin-bottom:4px;">Export diagram as</div>');
    [["svg", "SVG"], ["png", "PNG"], ["jpeg", "JPEG"]].forEach(function (f) {
      $('<button type="button" class="btn btn-default btn-xs" style="display:block; width:100%; margin-bottom:3px;"></button>')
        .text(f[1]).appendTo(menu)
        .on("click", function (ev) { ev.stopPropagation(); closeMenu(); exportAs(svg, f[0]); });
    });
    $("body").append(menu);
  });

  $(document).on("click", function () { closeMenu(); });
})();
