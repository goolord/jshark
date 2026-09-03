import { fileURLToPath } from "node:url";
import path from "node:path";
import puppeteer from "puppeteer-core";

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, "..");
const BASE = process.env.SHOT_BASE || "http://127.0.0.1:3000";
const OUT = process.env.SHOT_OUT || path.join(root, "examples/static/img");
const CHROME = process.env.CHROME || "/usr/bin/chromium";

const hideSourceCss = `
  .source-stack, .js-source, .life-source { display: none !important; }
`;

function canvasHasColor(page, selector) {
  return page.evaluate((sel) => {
    const c = document.querySelector(sel);
    if (!c || !(c instanceof HTMLCanvasElement)) return false;
    const w = Math.min(c.width, 64);
    const h = Math.min(c.height, 64);
    if (w < 2 || h < 2) return false;
    const ctx = c.getContext("2d", { willReadFrequently: true });
    if (!ctx) return false;
    try {
      const { data } = ctx.getImageData(0, 0, w, h);
      for (let i = 0; i < data.length; i += 4) {
        if (
          data[i + 3] > 8 &&
          (data[i] > 20 || data[i + 1] > 20 || data[i + 2] > 20)
        ) {
          return true;
        }
      }
    } catch {
      return false;
    }
    return false;
  }, selector);
}

async function waitFor(fn, timeoutMs, stepMs = 200) {
  const t0 = Date.now();
  while (Date.now() - t0 < timeoutMs) {
    if (await fn()) return true;
    await new Promise((r) => setTimeout(r, stepMs));
  }
  return false;
}

async function hideSource(page) {
  await page.addStyleTag({ content: hideSourceCss });
}

async function screenshotEl(page, selector, dest) {
  const el = await page.$(selector);
  if (!el) throw new Error(`missing ${selector}`);
  await el.screenshot({ path: dest, type: "png" });
}

async function seedTodos(page) {
  const input = await page.$("#new-todo, .new-todo");
  if (!input) throw new Error("todo input missing");
  const items = ["Taste JavaScript", "Buy a unicorn", "Write JShark"];
  for (const title of items) {
    await input.click({ clickCount: 3 });
    await input.type(title);
    await page.keyboard.press("Enter");
    await new Promise((r) => setTimeout(r, 80));
  }
  const toggle = await page.$(".todo-list li .toggle");
  if (toggle) await toggle.click();
}

const jobs = [
  {
    name: "breakout",
    path: "/breakout",
    viewport: { width: 720, height: 640 },
    scale: 2,
    ready: async (page) => canvasHasColor(page, "#board"),
    settleMs: 1200,
    shot: (page) => screenshotEl(page, "main.page", path.join(OUT, "breakout.png")),
  },
  {
    name: "todo-mvc",
    path: "/todo-mvc",
    viewport: { width: 800, height: 900 },
    scale: 2,
    ready: async () => true,
    afterReady: seedTodos,
    shot: async (page) => {
      // Classic TodoMVC "todos" h1 sits at top: -140px outside .todoapp.
      const box = await page.evaluate(() => {
        const app = document.querySelector(".todoapp");
        const info = document.querySelector(".info");
        if (!app) return null;
        const a = app.getBoundingClientRect();
        const b = info ? info.getBoundingClientRect() : a;
        const left = Math.min(a.left, b.left);
        const right = Math.max(a.right, b.right);
        const bottom = Math.max(a.bottom, b.bottom);
        const pad = 24;
        const titleLift = 160;
        return {
          x: Math.max(0, left - pad),
          y: Math.max(0, a.top - titleLift),
          width: right - left + pad * 2,
          height: bottom - (a.top - titleLift) + pad,
        };
      });
      if (!box) throw new Error("todoapp missing");
      await page.screenshot({
        path: path.join(OUT, "todo-mvc.png"),
        clip: box,
        type: "png",
      });
    },
  },
  {
    name: "synth",
    path: "/synth",
    viewport: { width: 1100, height: 820 },
    scale: 2,
    ready: async (page) => page.$("#keyboard") != null,
    shot: (page) =>
      screenshotEl(page, "main.page.synth", path.join(OUT, "synth.png")),
  },
  {
    name: "life",
    path: "/life",
    viewport: { width: 1280, height: 900 },
    scale: 1,
    ready: async (page) => {
      const frame = page.frames().find((f) => f.url().includes("/life/frame"));
      if (!frame) return false;
      return frame
        .evaluate(() => {
          const el = document.querySelector("#life-stat-gen");
          if (el && /\d/.test(el.textContent || "")) return true;
          const c =
            document.querySelector("#board") ||
            document.querySelector("canvas");
          return c instanceof HTMLCanvasElement && c.width > 0 && c.height > 0;
        })
        .catch(() => false);
    },
    shot: async (page) => {
      await page.screenshot({
        path: path.join(OUT, "life.png"),
        type: "png",
      });
    },
  },
  {
    name: "hvm2-demo",
    path: "/hvm2-demo",
    viewport: { width: 1100, height: 980 },
    scale: 2,
    ready: async (page) => {
      const backend = await page
        .$eval("#hvm2-metric-backend", (el) => (el.textContent || "").trim())
        .catch(() => "");
      if (backend && backend !== "…" && backend !== "...") return true;
      return canvasHasColor(page, "#hvm2-canvas");
    },
    shot: (page) =>
      screenshotEl(page, "main.page.hvm2", path.join(OUT, "hvm2-demo.png")),
  },
];

const browser = await puppeteer.launch({
  executablePath: CHROME,
  headless: "new",
  args: [
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--hide-scrollbars",
    "--use-angle=swiftshader",
    "--enable-webgl",
    "--ignore-gpu-blocklist",
    "--autoplay-policy=no-user-gesture-required",
  ],
});

try {
  for (const job of jobs) {
    const page = await browser.newPage();
    await page.setViewport({
      width: job.viewport.width,
      height: job.viewport.height,
      deviceScaleFactor: job.scale ?? 2,
    });
    const url = `${BASE}${job.path}`;
    console.log(`open ${url}`);
    await page.goto(url, { waitUntil: "networkidle0", timeout: 60000 });
    await hideSource(page);
    const ok = await waitFor(() => job.ready(page), 20000);
    if (!ok) console.warn(`warn: ${job.name} ready timed out`);
    if (job.afterReady) await job.afterReady(page);
    await new Promise((r) => setTimeout(r, job.settleMs ?? 400));
    await job.shot(page);
    await page.close();
    console.log(`wrote ${path.join(OUT, job.name + ".png")}`);
  }
} finally {
  await browser.close();
}
