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
      this.wasmStep = null;
      this.gridA = null;
      this.gridB = null;
      this.sab = null;
      this.sync = null;
      this.ready = false;
      this.workerUrl = 'js/EngineWorker.js';
      this._speciesCounts = new Uint16Array(256);
      this._speciesTouched = new Uint16Array(8);
    }

    /** Optional WASM SIMD hook: pass a module with exports.stepRegion(ptr,w,h,y0,y1). */
    async loadWasm(url) {
      if (!global.WebAssembly) return false;
      try {
        const resp = await fetch(url);
        const bytes = await resp.arrayBuffer();
        const { instance } = await WebAssembly.instantiate(bytes, {});
        if (typeof instance.exports.stepRegion === 'function') {
          this.wasmStep = instance.exports.stepRegion;
          return true;
        }
      } catch (_) {
        /* no wasm bundle shipped */
      }
      return false;
    }

    init(opts) {
      if (!LifeLUT) {
        console.warn('[LifeEngine] LUTGenerator.js must load before Main.js');
        this.ready = false;
        this.mode = 'none';
        return this.mode;
      }
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
        this.workers = [];
        this.gridA = new Uint8Array(n);
        this.gridB = new Uint8Array(n);
        this.mode = 'main-lut';
      }
      this.ready = true;
      return this.mode;
    }

    spawnWorkers(pool) {
      const tileH = Math.ceil(this.height / pool);
      this.workers = [];
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
        wasm: !!this.wasmStep,
      };
    }

    setRenderMs(ms) {
      this.lastRenderMs = ms;
    }
  }

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
   */
  function finishStep(alive, species, nextAlive, nextSpecies, w, h, nextLiveList, nextChangedList) {
    const E = global.LifeEngine;
    if (!E || !E.ready || E.mode === 'none') return null;
    const n = w * h | 0;
    const grid = E.gridA;
    const counts = E._speciesCounts;
    const touched = E._speciesTouched;
    for (let i = 0; i < n; i++) grid[i] = alive[i] & 1;
    E.stepMainLUT();
    let pop = 0;
    let bx0 = 1e9;
    let by0 = 1e9;
    let bx1 = -1;
    let by1 = -1;
    for (let i = 0; i < n; i++) {
      const was = alive[i] & 1;
      const now = grid[i] & 1;
      if (now && was) {
        nextAlive[i] = 1;
        nextSpecies[i] = species[i];
      } else if (!now) {
        nextAlive[i] = 0;
        nextSpecies[i] = 0;
      } else {
        const x = i % w;
        const y = (i / w) | 0;
        nextAlive[i] = 1;
        nextSpecies[i] = pickBirthSpecies(alive, species, w, h, x, y, counts, touched);
      }
      if (now) {
        pop++;
        const x = i % w;
        const y = (i / w) | 0;
        if (x < bx0) bx0 = x;
        if (y < by0) by0 = y;
        if (x > bx1) bx1 = x;
        if (y > by1) by1 = y;
        nextLiveList.push(i);
        if (was !== now) nextChangedList.push(i);
      } else if (was) {
        nextChangedList.push(i);
      }
    }
    rebuildPackedCounts(nextAlive, w, h);
    return { pop, bx0, by0, bx1, by1 };
  }

  global.LifeEngine = new LifeEngineMain();
  global.LifeEngineSync = { rebuildPackedCounts, finishStep };
})(globalThis);
