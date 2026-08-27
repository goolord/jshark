// Import path must match DevServer.speedHighlightPrefix under /static/.
import { highlightElement } from "./speed-highlight/index.js";

function langFromClass(code) {
  var m = (code.className || "").match(/shj-lang-(\S+)/);
  return m ? m[1] : "js";
}

function scheduleIdle(fn) {
  if (typeof requestIdleCallback === "function") {
    requestIdleCallback(fn, { timeout: 3000 });
  } else {
    setTimeout(fn, 0);
  }
}

async function maybeHighlight(code) {
  if (!code || code.dataset.highlighted) return;
  var lang = langFromClass(code);
  if (lang === "plain") {
    code.dataset.highlighted = "1";
    return;
  }
  var text = code.textContent || "";
  if (
    !text ||
    text === "Expand to load source…" ||
    text === "Loading source…"
  ) {
    return;
  }
  var run = async function () {
    try {
      await highlightElement(code, lang, { showLineNumbers: false });
      code.dataset.highlighted = "1";
    } catch (e) {
      console.error("speed-highlight failed", e);
    }
  };
  if (text.length > 131072) {
    scheduleIdle(function () {
      run().catch(function (e) {
        console.error("speed-highlight failed", e);
      });
    });
  } else {
    await run();
  }
}

function highlightPane(pane) {
  var code = pane.querySelector("code");
  if (code) {
    maybeHighlight(code).catch(function (e) {
      console.error("speed-highlight failed", e);
    });
  }
}

window.jsharkHighlightCode = maybeHighlight;

window.jsharkWhenHighlightReady = function (fn) {
  if (window.jsharkHighlightCode) {
    fn(window.jsharkHighlightCode);
    return;
  }
  var tries = 0;
  (function poll() {
    if (window.jsharkHighlightCode) {
      fn(window.jsharkHighlightCode);
    } else if (++tries < 200) {
      setTimeout(poll, 25);
    } else {
      console.error("source-pane.js did not load");
    }
  })();
};

document.querySelectorAll(".js-source, .life-source").forEach(function (pane) {
  pane.addEventListener("toggle", function () {
    if (pane.open) highlightPane(pane);
  });
  if (pane.open) highlightPane(pane);
});

document.querySelectorAll(".js-source, .life-source").forEach(function (pane) {
  var btn = pane.querySelector(".js-source-copy");
  var code = pane.querySelector("code");
  if (!btn || !code) return;
  btn.addEventListener("click", function (e) {
    e.preventDefault();
    e.stopPropagation();
    var text = code.textContent || "";
    if (
      !text ||
      text === "Loading source…" ||
      text === "Expand to load source…"
    ) {
      return;
    }
    navigator.clipboard
      .writeText(text)
      .then(function () {
        btn.textContent = "Copied!";
        btn.disabled = true;
        setTimeout(function () {
          btn.textContent = "Copy";
          btn.disabled = false;
        }, 1500);
      })
      .catch(function () {
        btn.textContent = "Copy failed";
        setTimeout(function () {
          btn.textContent = "Copy";
        }, 1500);
      });
  });
});
