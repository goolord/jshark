/*! jshark hot-reload client — EventSource + CSS/JS swap + debug panel */
(function () {
  "use strict";
  if (window.__JSHARK_HR_BOOT__) return;
  window.__JSHARK_HR_BOOT__ = true;

  var EVENTS_URL = "/__jshark/events";
  var STORAGE_KEY = "__jshark_hr_hashes";
  var es = null;
  var retryMs = 500;
  var maxRetryMs = 8000;
  var fetchCtl = null;
  var applySeq = 0;
  var gen = 0;
  var tracking = false;
  var rafs = [];
  var intervals = [];
  var timeouts = [];
  var listeners = [];
  var panel = null;
  var panelEls = null;
  var collapsed = false;
  var dbg = {
    sse: "connecting",
    compiling: "",
    applying: false,
    lastType: "boot",
    lastDetail: "",
    error: "",
    hash: ""
  };

  var origRaf = window.requestAnimationFrame.bind(window);
  var origCaf = window.cancelAnimationFrame.bind(window);
  var origSetInterval = window.setInterval.bind(window);
  var origClearInterval = window.clearInterval.bind(window);
  var origSetTimeout = window.setTimeout.bind(window);
  var origClearTimeout = window.clearTimeout.bind(window);
  var origAdd = EventTarget.prototype.addEventListener;
  var origRemove = EventTarget.prototype.removeEventListener;
  var windowAdd = window.addEventListener;
  var windowRemove = window.removeEventListener;
  var docAdd = document.addEventListener;
  var docRemove = document.removeEventListener;

  function dropId(arr, id) {
    var i = arr.indexOf(id);
    if (i >= 0) arr.splice(i, 1);
  }

  function shouldTrackTarget(t) {
    if (!t) return false;
    if (typeof EventSource !== "undefined" && t instanceof EventSource) {
      return false;
    }
    if (typeof WebSocket !== "undefined" && t instanceof WebSocket) {
      return false;
    }
    return true;
  }

  // foreverFrame / setTimeout(0) chains schedule the next tick after
  // module eval. Keep tracking on so those nested calls stay disposable.
  window.requestAnimationFrame = function (cb) {
    if (!tracking) return origRaf(cb);
    var g = gen;
    var id = origRaf(function (ts) {
      dropId(rafs, id);
      if (g !== gen) return;
      if (typeof cb === "function") cb(ts);
    });
    rafs.push(id);
    return id;
  };
  window.cancelAnimationFrame = function (id) {
    dropId(rafs, id);
    return origCaf(id);
  };
  window.setInterval = function (cb) {
    if (!tracking) return origSetInterval.apply(window, arguments);
    var args = Array.prototype.slice.call(arguments);
    var g = gen;
    if (typeof cb === "function") {
      args[0] = function () {
        if (g !== gen) return;
        cb.apply(this, arguments);
      };
    }
    var id = origSetInterval.apply(window, args);
    intervals.push(id);
    return id;
  };
  window.clearInterval = function (id) {
    dropId(intervals, id);
    return origClearInterval(id);
  };
  window.setTimeout = function (cb) {
    if (!tracking) return origSetTimeout.apply(window, arguments);
    var args = Array.prototype.slice.call(arguments);
    var g = gen;
    var id;
    if (typeof cb === "function") {
      args[0] = function () {
        dropId(timeouts, id);
        if (g !== gen) return;
        cb.apply(this, arguments);
      };
    }
    id = origSetTimeout.apply(window, args);
    timeouts.push(id);
    return id;
  };
  window.clearTimeout = function (id) {
    dropId(timeouts, id);
    return origClearTimeout(id);
  };
  function makeAdd(hostAdd, hostRemove) {
    return function (type, listener, options) {
      if (!tracking || listener == null || !shouldTrackTarget(this)) {
        return hostAdd.call(this, type, listener, options);
      }
      var g = gen;
      var wrapped;
      if (typeof listener === "function") {
        wrapped = function (ev) {
          if (g !== gen) return;
          return listener.call(this, ev);
        };
      } else if (typeof listener.handleEvent === "function") {
        wrapped = function (ev) {
          if (g !== gen) return;
          return listener.handleEvent(ev);
        };
      } else {
        return hostAdd.call(this, type, listener, options);
      }
      listeners.push({
        t: this,
        type: type,
        orig: listener,
        wrapped: wrapped,
        opt: options,
        remove: hostRemove
      });
      return hostAdd.call(this, type, wrapped, options);
    };
  }

  function makeRemove(hostRemove) {
    return function (type, listener, options) {
      for (var i = listeners.length - 1; i >= 0; i--) {
        var item = listeners[i];
        if (item.t === this && item.type === type && item.orig === listener) {
          item.remove.call(this, type, item.wrapped, item.opt);
          listeners.splice(i, 1);
          return;
        }
      }
      return hostRemove.call(this, type, listener, options);
    };
  }

  EventTarget.prototype.addEventListener = makeAdd(origAdd, origRemove);
  EventTarget.prototype.removeEventListener = makeRemove(origRemove);
  if (windowAdd !== origAdd) {
    window.addEventListener = makeAdd(windowAdd, windowRemove);
    window.removeEventListener = makeRemove(windowRemove);
  }
  if (docAdd !== origAdd) {
    document.addEventListener = makeAdd(docAdd, docRemove);
    document.removeEventListener = makeRemove(docRemove);
  }

  function disposeTracked() {
    gen += 1;
    while (rafs.length) origCaf(rafs.pop());
    while (intervals.length) origClearInterval(intervals.pop());
    while (timeouts.length) origClearTimeout(timeouts.pop());
    while (listeners.length) {
      var item = listeners.pop();
      try {
        item.remove.call(item.t, item.type, item.wrapped, item.opt);
      } catch (_) {}
    }
  }

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

  function appNameFromPath() {
    var path = location.pathname.replace(/\/$/, "");
    return path.split("/").filter(Boolean).pop() || "";
  }

  function el(tag, cls, text) {
    var node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text) node.textContent = text;
    return node;
  }

  function mountPanel() {
    if (panel) return panel;
    if (!document.getElementById("__jshark-hr-css")) {
      var style = document.createElement("style");
      style.id = "__jshark-hr-css";
      style.textContent =
        "#__jshark-hr-panel{position:fixed;right:12px;bottom:12px;z-index:2147483647;" +
        "width:268px;max-width:calc(100vw - 24px);color:#f3ead8;background:#1a1714;" +
        "border:1px solid #4a3f32;box-shadow:5px 6px 0 #0c0a08;" +
        "font:11px/1.4 ui-monospace,'Cascadia Mono',Consolas,monospace;" +
        "letter-spacing:.02em}" +
        "#__jshark-hr-panel header{display:flex;align-items:center;gap:8px;" +
        "padding:7px 10px;cursor:pointer;user-select:none;border-bottom:1px solid #4a3f32}" +
        "#__jshark-hr-panel .wick{width:7px;height:16px;flex:0 0 auto;background:#3ecfbf;" +
        "box-shadow:inset 0 0 0 1px #0c0a08}" +
        "#__jshark-hr-panel .brand{flex:1;font-weight:700;letter-spacing:.14em;" +
        "font-size:10px;color:#c4b49a}" +
        "#__jshark-hr-panel .phase{text-transform:uppercase;letter-spacing:.08em;font-size:10px}" +
        "#__jshark-hr-panel dl{margin:0;padding:8px 10px 10px;display:grid;" +
        "grid-template-columns:64px 1fr;gap:3px 8px}" +
        "#__jshark-hr-panel dt{color:#8d7b63;margin:0}" +
        "#__jshark-hr-panel dd{margin:0;color:#f3ead8;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}" +
        "#__jshark-hr-panel pre{margin:0;padding:8px 10px;max-height:12em;overflow:auto;" +
        "white-space:pre-wrap;word-break:break-word;background:#2a1612;color:#ffd2c4;" +
        "border-top:1px solid #6a3a32;display:none}" +
        "#__jshark-hr-panel.is-wait .wick{background:#e8a54b;animation:jsharkWick .85s steps(2,end) infinite}" +
        "#__jshark-hr-panel.is-apply .wick{background:#7ec8ff}" +
        "#__jshark-hr-panel.is-err .wick{background:#e85d4c}" +
        "#__jshark-hr-panel.is-down .wick{background:#8d7b63}" +
        "#__jshark-hr-panel.is-wait .phase{color:#e8a54b}" +
        "#__jshark-hr-panel.is-apply .phase{color:#7ec8ff}" +
        "#__jshark-hr-panel.is-err .phase{color:#e85d4c}" +
        "#__jshark-hr-panel.is-down .phase{color:#c4b49a}" +
        "#__jshark-hr-panel.is-live .phase{color:#3ecfbf}" +
        "#__jshark-hr-panel.is-err pre{display:block}" +
        "#__jshark-hr-panel.is-collapsed{width:auto}" +
        "#__jshark-hr-panel.is-collapsed header{border-bottom:0}" +
        "#__jshark-hr-panel.is-collapsed dl,#__jshark-hr-panel.is-collapsed pre{display:none}" +
        "@media (prefers-reduced-motion:reduce){#__jshark-hr-panel.is-wait .wick{animation:none}}" +
        "@keyframes jsharkWick{50%{opacity:.28}}";
      document.documentElement.appendChild(style);
    }
    panel = el("aside");
    panel.id = "__jshark-hr-panel";
    panel.setAttribute("aria-live", "polite");
    var head = el("header");
    head.setAttribute("role", "button");
    head.setAttribute("tabindex", "0");
    var wick = el("span", "wick");
    var brand = el("span", "brand", "JSHARK");
    var phase = el("span", "phase", "boot");
    head.appendChild(wick);
    head.appendChild(brand);
    head.appendChild(phase);
    var dl = el("dl");
    function row(key) {
      var dt = el("dt", "", key);
      var dd = el("dd");
      dd.dataset.k = key;
      dl.appendChild(dt);
      dl.appendChild(dd);
      return dd;
    }
    var ddApp = row("app");
    var ddSse = row("sse");
    var ddLast = row("last");
    var ddHash = row("hash");
    var ddGen = row("gen");
    var pre = el("pre");
    pre.setAttribute("role", "alert");
    panel.appendChild(head);
    panel.appendChild(dl);
    panel.appendChild(pre);
    document.documentElement.appendChild(panel);
    origAdd.call(head, "click", function () {
      collapsed = !collapsed;
      paint();
    });
    origAdd.call(head, "keydown", function (ev) {
      if (ev.key === "Enter" || ev.key === " ") {
        ev.preventDefault();
        collapsed = !collapsed;
        paint();
      }
    });
    panelEls = {
      phase: phase,
      app: ddApp,
      sse: ddSse,
      last: ddLast,
      hash: ddHash,
      gen: ddGen,
      pre: pre
    };
    return panel;
  }

  function phaseOf() {
    if (dbg.error) return "error";
    if (dbg.compiling) return "compiling";
    if (dbg.applying) return "applying";
    if (dbg.sse === "retry") return "reconnect";
    if (dbg.sse === "connecting") return "connecting";
    return "live";
  }

  function modeOf(phase) {
    if (phase === "error") return "err";
    if (phase === "compiling") return "wait";
    if (phase === "applying") return "apply";
    if (phase === "reconnect" || phase === "connecting") return "down";
    return "live";
  }

  function paint() {
    mountPanel();
    var phase = phaseOf();
    panel.className =
      "is-" +
      modeOf(phase) +
      (collapsed ? " is-collapsed" : "") +
      (dbg.error ? " is-err" : "");
    var head = panel.querySelector("header");
    if (head) head.setAttribute("aria-expanded", collapsed ? "false" : "true");
    panelEls.phase.textContent = phase;
    panelEls.app.textContent = dbg.compiling || appNameFromPath() || "-";
    panelEls.sse.textContent =
      dbg.sse === "retry" ? "retry " + retryMs + "ms" : dbg.sse;
    panelEls.last.textContent = dbg.lastDetail
      ? dbg.lastType + " / " + dbg.lastDetail
      : dbg.lastType;
    panelEls.hash.textContent = dbg.hash || "-";
    panelEls.gen.textContent = String(gen);
    if (dbg.error) {
      panelEls.pre.textContent = dbg.error;
    } else {
      panelEls.pre.textContent = "";
    }
  }

  function noteEvent(type, detail) {
    dbg.lastType = type;
    dbg.lastDetail = detail || "";
    paint();
  }

  function setError(msg) {
    dbg.error = msg ? String(msg) : "";
    if (msg) {
      dbg.compiling = "";
      dbg.applying = false;
    }
    paint();
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

  function currentAppUrl() {
    var scripts = document.querySelectorAll(
      "script[src*='/app.js'], script[data-jshark-app]"
    );
    if (scripts.length) return scripts[0].getAttribute("src") || "";
    var name = appNameFromPath();
    return name ? "/" + name + "/app.js" : "";
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
    disposeTracked();
    if (es) bindEs();
  }

  function runModuleScript(src, seq) {
    return new Promise(function (resolve, reject) {
      if (seq !== applySeq) {
        resolve();
        return;
      }
      var blob = new Blob([src], { type: "text/javascript" });
      var blobUrl = URL.createObjectURL(blob);
      var tag = document.createElement("script");
      tag.async = false;
      tag.src = blobUrl;
      function finish(err) {
        try {
          URL.revokeObjectURL(blobUrl);
        } catch (_) {}
        if (tag.parentNode) tag.parentNode.removeChild(tag);
        if (err) reject(err);
        else resolve();
      }
      tag.onload = function () {
        finish(null);
      };
      tag.onerror = function () {
        finish(new Error("hmr script"));
      };
      tracking = true;
      (document.head || document.documentElement).appendChild(tag);
    });
  }

  function applyHmr(url, hash) {
    var srcUrl = url || currentAppUrl();
    var base = srcUrl.split("?")[0];
    if (!base) {
      location.reload();
      return;
    }
    var seq = (applySeq += 1);
    if (fetchCtl) {
      try {
        fetchCtl.abort();
      } catch (_) {}
    }
    fetchCtl =
      typeof AbortController !== "undefined" ? new AbortController() : null;
    var busted = base + "?_hr=" + encodeURIComponent(hash || String(Date.now()));
    var opts = { cache: "no-store", credentials: "same-origin" };
    if (fetchCtl) opts.signal = fetchCtl.signal;
    dbg.compiling = "";
    dbg.applying = true;
    dbg.hash = hash || "";
    dbg.error = "";
    noteEvent("hmr", hash || base);
    fetch(busted, opts)
      .then(function (res) {
        if (!res.ok) throw new Error("hmr " + res.status);
        return res.text();
      })
      .then(function (src) {
        if (seq !== applySeq) return;
        disposeApp();
        return runModuleScript(src, seq);
      })
      .then(function () {
        if (seq !== applySeq) return;
        dbg.applying = false;
        paint();
      })
      .catch(function (err) {
        if (err && err.name === "AbortError") return;
        dbg.applying = false;
        paint();
        console.warn("[jshark-hr] HMR failed; full reload", err);
        location.reload();
      });
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
    noteEvent("hello", keys.length ? keys.join(",") : "ok");
    if (changed.length === 0) {
      return;
    }
    dbg.error = "";
    var name = appNameFromPath();
    if (name && incoming[name]) {
      applyHmr("/" + name + "/app.js", incoming[name]);
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
      case "build-start":
        dbg.compiling = payload.appName || appNameFromPath();
        dbg.applying = false;
        dbg.error = "";
        noteEvent("build-start", dbg.compiling);
        break;
      case "css-update":
        dbg.error = "";
        bustCss(payload.url, payload.timestamp);
        noteEvent("css-update", payload.url || "");
        break;
      case "js-update":
        dbg.error = "";
        dbg.compiling = "";
        (function () {
          var hashes = loadHashes();
          if (payload.appName && payload.hash) hashes[payload.appName] = payload.hash;
          saveHashes(hashes);
          applyHmr(payload.url, payload.hash);
        })();
        break;
      case "page-reload":
        dbg.compiling = "";
        noteEvent("page-reload", payload.reason || "");
        location.reload();
        break;
      case "build-error":
        setError(payload.message);
        noteEvent("build-error", "");
        break;
      default:
        break;
    }
  }

  function bindEs() {
    if (!es) return;
    es.onopen = function () {
      retryMs = 500;
      dbg.sse = "live";
      paint();
    };
    es.onmessage = onMessage;
    es.onerror = function () {
      try {
        es.close();
      } catch (_) {}
      es = null;
      dbg.sse = "retry";
      paint();
      origSetTimeout(connect, retryMs);
      retryMs = Math.min(maxRetryMs, Math.floor(retryMs * 1.5));
    };
  }

  function connect() {
    if (es) {
      try {
        es.close();
      } catch (_) {}
    }
    dbg.sse = "connecting";
    paint();
    es = new EventSource(EVENTS_URL);
    bindEs();
  }

  window.__JSHARK_HR_API__ = {
    disposeTracked: disposeTracked,
    shouldTrackTarget: shouldTrackTarget,
    eventSource: function () {
      return es;
    },
    untracked: function (fn) {
      var prev = tracking;
      tracking = false;
      try {
        return fn();
      } finally {
        tracking = prev;
      }
    },
    withModule: function (fn) {
      tracking = true;
      return fn();
    }
  };

  // Track the page's app.js (it runs after this file when we sit in <head>).
  tracking = true;
  mountPanel();
  paint();
  if (document.readyState === "loading") {
    origAdd.call(document, "DOMContentLoaded", connect);
  } else {
    connect();
  }
})();
