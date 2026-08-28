import puppeteer from "puppeteer-core";

const CHROME = process.env.CHROME || "/usr/bin/chromium";
const URL = process.env.LIFE_URL || "http://127.0.0.1:3000/life/frame/";
const MS = Number(process.env.LIFE_MS || 8000);

function pct(sorted, q) {
  if (sorted.length === 0) return null;
  const i = Math.min(sorted.length - 1, Math.ceil(q * (sorted.length - 1)));
  return sorted[i];
}

function summarize(raw) {
  const frames = raw?.frames || [];
  const fps = frames.map((f) => f.fps).filter((n) => Number.isFinite(n) && n > 0);
  const step = frames.map((f) => f.stepMs).filter((n) => Number.isFinite(n));
  const render = frames.map((f) => f.renderMs).filter((n) => Number.isFinite(n));
  const other = frames.map((f) => f.otherMs).filter((n) => Number.isFinite(n));
  const lost = frames.filter((f) => f.glLost).length;
  const sort = (xs) => xs.slice().sort((a, b) => a - b);
  const fpsS = sort(fps);
  const stepS = sort(step);
  const renderS = sort(render);
  const otherS = sort(other);
  const last = raw?.last || frames[frames.length - 1] || {};
  return {
    glRenderer: raw?.glRenderer || "",
    glLost: raw?.glLost || 0,
    frames: frames.length,
    gen: last.gen ?? null,
    pop: last.pop ?? null,
    lostFrames: lost,
    lostPct: frames.length ? Math.round((1000 * lost) / frames.length) / 10 : 0,
    fps: { p50: pct(fpsS, 0.5), p95: pct(fpsS, 0.95), min: fpsS[0] ?? null },
    stepMs: {
      p50: pct(stepS, 0.5),
      p95: pct(stepS, 0.95),
      max: stepS[stepS.length - 1] ?? null,
    },
    renderMs: {
      p50: pct(renderS, 0.5),
      p95: pct(renderS, 0.95),
      max: renderS[renderS.length - 1] ?? null,
    },
    otherMs: {
      p50: pct(otherS, 0.5),
      p95: pct(otherS, 0.95),
      max: otherS[otherS.length - 1] ?? null,
    },
  };
}

const headed = process.env.LIFE_HEADED === "1";
const browser = await puppeteer.launch({
  executablePath: CHROME,
  headless: headed ? false : "new",
  args: [
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--hide-scrollbars",
    "--enable-webgl",
    "--ignore-gpu-blocklist",
    "--autoplay-policy=no-user-gesture-required",
  ],
});

try {
  const page = await browser.newPage();
  page.on("console", (msg) => {
    const t = msg.text();
    if (/WebGL|Life|GL/i.test(t)) {
      console.error(`console: ${t}`);
    }
  });
  await page.setViewport({ width: 1280, height: 900, deviceScaleFactor: 1 });
  console.error(`open ${URL}`);
  await page.goto(URL, { waitUntil: "domcontentloaded", timeout: 60000 });
  await page.waitForFunction(
    () => window.__jsharkLifeProfile && window.__jsharkLifeProfile.frames.length > 2,
    { timeout: 20000 },
  );
  await new Promise((r) => setTimeout(r, MS));
  const raw = await page.evaluate(() => window.__jsharkLifeProfile);
  if (!raw) throw new Error("missing window.__jsharkLifeProfile");
  const out = summarize(raw);
  console.log(JSON.stringify(out, null, 2));
} finally {
  await browser.close();
}
