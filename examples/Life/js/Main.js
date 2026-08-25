'use strict';

/**
 * Life engine orchestrator: LUT stepping for sync main-thread finishStep,
 * optional worker pool (asyncStep only), and optional WASM hook.
 * Exposes globalThis.LifeEngine.
 */
(function (global) {
  const LifeLUT = global.LifeLUT;
  const SYNC_WORDS = 8;
  const SYNC_BYTES = SYNC_WORDS * 4;

  class LifeEngineMain {
    constructor() {
      this.width = 0;
      this.height = 0;
      this.LUT = null;
      this.workers = [];
      this.mode = 'none';
      this.lastTickMs = 0;
      this.lastRenderMs = 0;
      this.wasmSimd = false;
      this.gridA = null;
      this.gridB = null;
      this.sab = null;
      this.sync = null;
      this.ready = false;
      this.workerUrl = 'js/EngineWorker.js';
      this._speciesCounts = new Uint16Array(256);
      this._speciesTouched = new Uint16Array(8);
      this._stepOut = { pop: 0, bx0: 0, by0: 0, bx1: 0, by1: 0 };
    }

    /** Load WASM SIMD kernels (row clear/copy). Stepping logic stays in LifeLUT. */
    async loadWasm(url) {
      if (this.mode === 'workers') {
        this._wasmReady = true;
        return false;
      }
      const LifeSimd = global.LifeSimd;
      if (!LifeSimd) {
        this._wasmReady = true;
        return false;
      }
      try {
        const ok = await LifeSimd.load(url, this.width, this.height);
        if (!ok) return false;
        if (!LifeSimd.bindGrids(this)) return false;
        this.wasmSimd = true;
        return true;
      } finally {
        this._wasmReady = true;
      }
    }

    init(opts) {
      if (!LifeLUT) {
        console.warn('[LifeEngine] LUTGenerator.js must load before Main.js');
        this.ready = false;
        this.mode = 'none';
        return this.mode;
      }
      this.terminateWorkers();
      this.width = opts.width | 0;
      this.height = opts.height | 0;
      this.workerUrl = opts.workerUrl || this.workerUrl;
      const n = this.width * this.height;
      this.LUT = LifeLUT.createLifeLUT();

      const canShare =
        typeof SharedArrayBuffer !== 'undefined' &&
        global.crossOriginIsolated === true;
      const wantWorkers =
        !!opts.asyncStep && opts.workerCount !== 0 && canShare;

      if (wantWorkers) {
        const gridBytes = n;
        const sabSize = SYNC_BYTES + gridBytes * 2;
        this.sab = new SharedArrayBuffer(sabSize);
        this.sync = new Int32Array(this.sab, 0, SYNC_WORDS);
        this.gridA = new Uint8Array(this.sab, SYNC_BYTES, gridBytes);
        this.gridB = new Uint8Array(this.sab, SYNC_BYTES + gridBytes, gridBytes);
        Atomics.store(this.sync, 4, this.width);
        Atomics.store(this.sync, 5, this.height);
        const pool = Math.max(1, opts.workerCount || navigator.hardwareConcurrency || 4);
        Atomics.store(this.sync, 3, pool);
        this.spawnWorkers(pool);
        this.mode = 'workers';
      } else {
        this.sab = null;
        this.sync = null;
        this.gridA = new Uint8Array(n);
        this.gridB = new Uint8Array(n);
        this.mode = 'main-lut';
      }
      this.ready = true;
      return this.mode;
    }

    terminateWorkers() {
      for (let i = 0; i < this.workers.length; i++) {
        this.workers[i].terminate();
      }
      this.workers = [];
    }

    spawnWorkers(pool) {
      this.terminateWorkers();
      const tileH = Math.ceil(this.height / pool);
      for (let id = 0; id < pool; id++) {
        const y0 = id * tileH;
        const y1 = Math.min(this.height, y0 + tileH);
        const worker = new Worker(this.workerUrl);
        worker.postMessage({
          type: 'init',
          sab: this.sab,
          workerId: id,
          y0,
          y1,
          w: this.width,
          h: this.height,
        });
        this.workers.push(worker);
      }
    }

    stepWorkers() {
      Atomics.store(this.sync, 2, 0);
      Atomics.store(this.sync, 1, 1);
      Atomics.add(this.sync, 0, 1);
      Atomics.notify(this.sync, 0, this.workers.length);
      while (Atomics.load(this.sync, 1) === 1) {
        /* worker-only path; asyncStep callers may use Atomics.waitAsync */
      }
      const tmp = this.gridA;
      this.gridA = this.gridB;
      this.gridB = tmp;
    }

    stepMainLUT() {
      LifeLUT.stepRegionLUT(
        this.LUT,
        this.gridA,
        this.gridB,
        this.width,
        this.height,
        0,
        this.height
      );
      const tmp = this.gridA;
      this.gridA = this.gridB;
      this.gridB = tmp;
    }

    step() {
      const t0 = global.performance.now();
      if (this.mode === 'workers') this.stepWorkers();
      else this.stepMainLUT();
      this.lastTickMs = global.performance.now() - t0;
      return this.lastTickMs;
    }

    getStats() {
      return {
        mode: this.mode,
        tickMs: this.lastTickMs,
        renderMs: this.lastRenderMs,
        workers: this.workers.length,
        wasm: !!this.wasmSimd,
      };
    }

    setRenderMs(ms) {
      this.lastRenderMs = ms;
    }
  }

  function refreshPackedRegion(grid, w, h, x0, y0, x1, y1) {
    w = w | 0;
    h = h | 0;
    if (w <= 0 || h <= 0 || !grid) return;
    const xs = Math.max(0, Math.floor(x0) - 1);
    const ys = Math.max(0, Math.floor(y0) - 1);
    const xe = Math.min(w - 1, Math.floor(x1) + 1);
    const ye = Math.min(h - 1, Math.floor(y1) + 1);
    if (xs > xe || ys > ye) return;
    for (let y = ys; y <= ye; y++) {
      const row = y * w;
      for (let x = xs; x <= xe; x++) {
        let n = 0;
        for (let dy = -1; dy <= 1; dy++) {
          for (let dx = -1; dx <= 1; dx++) {
            if (!dx && !dy) continue;
            const nx = x + dx;
            const ny = y + dy;
            if (nx < 0 || ny < 0 || nx >= w || ny >= h) continue;
            if (grid[ny * w + nx] & 1) n++;
          }
        }
        grid[row + x] = (grid[row + x] & 1) + n * 2;
      }
    }
  }

  function pickBirthSpecies(alive, species, w, h, x, y, counts, touched) {
    let best = 0;
    let bestSid = 0;
    let touchedLen = 0;
    for (let dy = -1; dy <= 1; dy++) {
      for (let dx = -1; dx <= 1; dx++) {
        if (!dx && !dy) continue;
        const nx = x + dx;
        const ny = y + dy;
        if (nx < 0 || ny < 0 || nx >= w || ny >= h) continue;
        const ni = ny * w + nx;
        if (!(alive[ni] & 1)) continue;
        const sid = species[ni];
        const c = ++counts[sid];
        if (c === 1) touched[touchedLen++] = sid;
        if (c > best) {
          best = c;
          bestSid = sid;
        }
      }
    }
    for (let k = 0; k < touchedLen; k++) counts[touched[k]] = 0;
    return bestSid;
  }

  /**
   * Binary step + species + pop/bounds/lists in one pass.
   * Returns null when LifeEngine is unavailable (caller keeps Haskell path).
   * Return value is a shallow copy of {@link LifeEngineMain#_stepOut}; do not
   * cache it across calls.
   */
  function finishStep(
    alive,
    species,
    nextAlive,
    nextSpecies,
    w,
    h,
    x0,
    y0,
    x1,
    y1,
    nextLiveList,
    nextChangedList
  ) {
    const E = global.LifeEngine;
    if (!E || !E.ready || E.mode === 'none') return null;
    w = w | 0;
    h = h | 0;
    if (w <= 0 || h <= 0) return null;
    const n = (w * h) | 0;
    const counts = E._speciesCounts;
    const touched = E._speciesTouched;

    const xStart = Math.max(0, Math.floor(x0) - 1);
    const yStart = Math.max(0, Math.floor(y0) - 1);
    const xStop = Math.min(w, Math.floor(x1) + 2);
    const yStop = Math.min(h, Math.floor(y1) + 2);
    const regionRows = yStop - yStart;
    const regionCols = xStop - xStart;
    const copyFull = regionRows * regionCols * 2 >= n;

    if (copyFull) {
      for (let i = 0; i < n; i++) E.gridA[i] = alive[i] & 1;
      E.stepMainLUT();
    } else {
      const copyY0 = Math.max(0, yStart - 1);
      const copyYStop = Math.min(h, yStop + 1);
      for (let y = copyY0; y < copyYStop; y++) {
        const row = y * w;
        for (let x = 0; x < w; x++) E.gridA[row + x] = alive[row + x] & 1;
      }
      // Pass yStart/yStop so stepRegionLUT applies row-0 / row-h-1 edge copies.
      LifeLUT.stepRegionLUT(E.LUT, E.gridA, E.gridB, w, h, yStart, yStop);
      const tmp = E.gridA;
      E.gridA = E.gridB;
      E.gridB = tmp;
    }
    const grid = E.gridA;

    let pop = 0;
    let bx0 = 1e9;
    let by0 = 1e9;
    let bx1 = -1;
    let by1 = -1;
    let liveLen = 0;
    let changedLen = 0;

    for (let y = yStart; y < yStop; y++) {
      const row = y * w;
      for (let x = xStart; x < xStop; x++) {
        const i = row + x;
        const was = alive[i] & 1;
        const now = grid[i] & 1;
        if (now && was) {
          nextAlive[i] = grid[i];
          nextSpecies[i] = species[i];
        } else if (!now) {
          nextAlive[i] = 0;
          nextSpecies[i] = 0;
        } else {
          nextAlive[i] = grid[i];
          nextSpecies[i] = pickBirthSpecies(alive, species, w, h, x, y, counts, touched);
        }
        if (now) {
          pop++;
          if (x < bx0) bx0 = x;
          if (y < by0) by0 = y;
          if (x > bx1) bx1 = x;
          if (y > by1) by1 = y;
          nextLiveList[liveLen++] = i;
          if (was !== now) nextChangedList[changedLen++] = i;
        } else if (was) {
          nextChangedList[changedLen++] = i;
        }
      }
    }

    nextLiveList.length = liveLen;
    nextChangedList.length = changedLen;
    if (pop > 0 && bx1 >= bx0 && by1 >= by0) {
      refreshPackedRegion(nextAlive, w, h, bx0, by0, bx1, by1);
    } else if (x1 >= x0 && y1 >= y0) {
      refreshPackedRegion(nextAlive, w, h, x0, y0, x1, y1);
    }
    const out = E._stepOut;
    out.pop = pop;
    out.bx0 = bx0;
    out.by0 = by0;
    out.bx1 = bx1;
    out.by1 = by1;
    return { pop: out.pop, bx0: out.bx0, by0: out.by0, bx1: out.bx1, by1: out.by1 };
  }

  global.LifeEngine = new LifeEngineMain();

  function rebuildPackedCounts(grid, w, h) {
    const n = w * h;
    for (let i = 0; i < n; i++) grid[i] &= 1;
    for (let y = 0; y < h; y++) {
      for (let x = 0; x < w; x++) {
        const i = y * w + x;
        if (grid[i] & 1) {
          for (let dy = -1; dy <= 1; dy++) {
            for (let dx = -1; dx <= 1; dx++) {
              if (!dx && !dy) continue;
              const nx = x + dx;
              const ny = y + dy;
              if (nx < 0 || ny < 0 || nx >= w || ny >= h) continue;
              grid[ny * w + nx] += 2;
            }
          }
        }
      }
    }
  }

  global.LifeEngineSync = { finishStep, rebuildPackedCounts };
})(globalThis);
