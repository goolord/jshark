/*! jshark hot-reload client — EventSource + CSS/JS swap + error HUD */
(function () {
  "use strict";
  if (window.__JSHARK_HR_BOOT__) return;
  window.__JSHARK_HR_BOOT__ = true;

  var EVENTS_URL = "/__jshark/events";
  var STORAGE_KEY = "__jshark_hr_hashes";
  var es = null;
  var retryMs = 500;
  var maxRetryMs = 8000;
  var hud = null;

  function loadHashes() {
    try {
      return JSON.parse(sessionStorage.getItem(STORAGE_KEY) || "{}") || {};
    } catch (_) {
      return {};
    }
  }

  function saveHashes(h) {
    try {
      sessionStorage.setItem(STORAGE_KEY, JSON.stringify(h));
    } catch (_) {}
  }

  function ensureHud() {
    if (hud) return hud;
    hud = document.createElement("div");
    hud.id = "__jshark-hr-hud";
    hud.setAttribute("role", "alert");
    hud.style.cssText =
      "display:none;position:fixed;left:0;right:0;bottom:0;z-index:2147483647;" +
      "max-height:40vh;overflow:auto;padding:12px 16px;box-sizing:border-box;" +
      "background:#3b0d0d;color:#ffd7d7;font:13px/1.4 ui-monospace,Consolas,monospace;" +
      "white-space:pre-wrap;border-top:2px solid #ff5555;";
    document.documentElement.appendChild(hud);
    return hud;
  }

  function showError(msg) {
    var el = ensureHud();
    el.textContent = "JShark build error\n\n" + String(msg || "");
    el.style.display = "block";
  }

  function hideError() {
    if (hud) hud.style.display = "none";
  }

  function bustCss(url, ts) {
    var links = document.querySelectorAll('link[rel="stylesheet"]');
    var target = url || "";
    for (var i = 0; i < links.length; i++) {
      var link = links[i];
      var href = link.getAttribute("href") || "";
      if (!href) continue;
      var base = href.split("?")[0];
      var match =
        !target ||
        base === target ||
        base.endsWith(target) ||
        target.endsWith(base.replace(/^\.\.\//, "/"));
      if (!match) continue;
      var next = base + "?_hr=" + (ts || Date.now());
      link.setAttribute("href", next);
    }
  }

  function findAppScripts() {
    return Array.prototype.slice.call(
      document.querySelectorAll("script[src*='/app.js'], script[data-jshark-app]")
    );
  }

  function disposeApp() {
    try {
      if (typeof window.__JSHARK_DISPOSE__ === "function") {
        window.__JSHARK_DISPOSE__();
      }
    } catch (e) {
      console.warn("[jshark-hr] dispose failed", e);
    }
    window.__JSHARK_DISPOSE__ = null;
    try {
      if (typeof window.__JSHARK_GET_STATE__ === "function") {
        window.__JSHARK_HOT_STATE__ = window.__JSHARK_GET_STATE__() || {};
      }
    } catch (e) {
      console.warn("[jshark-hr] getState failed", e);
    }
  }

  function remountScript(url, hash) {
    disposeApp();
    var scripts = findAppScripts();
    var src = url || (scripts[0] && scripts[0].getAttribute("src")) || "";
    if (!src) {
      location.reload();
      return;
    }
    var base = src.split("?")[0];
    var busted = base + "?_hr=" + encodeURIComponent(hash || String(Date.now()));
    scripts.forEach(function (s) {
      try {
        s.remove();
      } catch (_) {}
    });
    var s = document.createElement("script");
    s.src = busted;
    s.async = false;
    s.dataset.jsharkApp = "1";
    s.onerror = function () {
      console.warn("[jshark-hr] script load failed; full reload");
      location.reload();
    };
    document.body.appendChild(s);
  }

  function onHello(payload) {
    var incoming = payload.jsHashes || {};
    var prev = loadHashes();
    var keys = Object.keys(incoming);
    var changed = [];
    for (var i = 0; i < keys.length; i++) {
      var k = keys[i];
      if (prev[k] && prev[k] !== incoming[k]) changed.push(k);
    }
    saveHashes(incoming);
    if (changed.length === 0) {
      hideError();
      return;
    }
    hideError();
    var path = location.pathname.replace(/\/$/, "");
    var name = path.split("/").filter(Boolean).pop() || "";
    if (name && incoming[name]) {
      remountScript("/" + name + "/app.js", incoming[name]);
    } else {
      location.reload();
    }
  }

  function onMessage(ev) {
    var payload;
    try {
      payload = JSON.parse(ev.data);
    } catch (_) {
      return;
    }
    switch (payload.type) {
      case "hello":
        onHello(payload);
        break;
      case "css-update":
        hideError();
        bustCss(payload.url, payload.timestamp);
        break;
      case "js-update":
        hideError();
        (function () {
          var hashes = loadHashes();
          if (payload.appName && payload.hash) hashes[payload.appName] = payload.hash;
          saveHashes(hashes);
          remountScript(payload.url, payload.hash);
        })();
        break;
      case "page-reload":
        location.reload();
        break;
      case "build-error":
        showError(payload.message);
        break;
      default:
        break;
    }
  }

  function connect() {
    if (es) {
      try {
        es.close();
      } catch (_) {}
    }
    es = new EventSource(EVENTS_URL);
    es.onopen = function () {
      retryMs = 500;
    };
    es.onmessage = onMessage;
    es.onerror = function () {
      try {
        es.close();
      } catch (_) {}
      es = null;
      setTimeout(connect, retryMs);
      retryMs = Math.min(maxRetryMs, Math.floor(retryMs * 1.5));
    };
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", connect);
  } else {
    connect();
  }
})();
