'use strict';

/**
 * Web Worker tile loop: SharedArrayBuffer grids + Atomics generation barrier.
 * WASM SIMD (LifeSimd) applies only on the main thread; worker grids stay on
 * the SAB and use the JS fill/set fallbacks inside LifeLUT.stepRegionLUT.
 */
importScripts('LUTGenerator.js');

(function () {
  const LifeLUT = self.LifeLUT;
  let LUT = null;
  let gridA = null;
  let gridB = null;
  let sync = null;
  let y0 = 0;
  let y1 = 0;
  let w = 0;
  let h = 0;

  function stepTile() {
    if (!LUT) LUT = LifeLUT.createLifeLUT();
    LifeLUT.stepRegionLUT(LUT, gridA, gridB, w, h, y0, y1);
  }

  self.onmessage = function (ev) {
    const msg = ev.data;
    if (msg.type !== 'init') return;
    sync = new Int32Array(msg.sab, 0, 8);
    const headerBytes = 32;
    const gridBytes = msg.w * msg.h;
    gridA = new Uint8Array(msg.sab, headerBytes, gridBytes);
    gridB = new Uint8Array(msg.sab, headerBytes + gridBytes, gridBytes);
    y0 = msg.y0;
    y1 = msg.y1;
    w = msg.w;
    h = msg.h;
    LUT = LifeLUT.createLifeLUT();
    self.postMessage({ type: 'ready', workerId: msg.workerId });
    let lastGen = Atomics.load(sync, 0);
    for (;;) {
      Atomics.wait(sync, 0, lastGen);
      lastGen = Atomics.load(sync, 0);
      if (Atomics.load(sync, 1) !== 1) continue;
      stepTile();
      const finished = Atomics.add(sync, 2, 1) + 1;
      const total = Atomics.load(sync, 3);
      if (finished === total) {
        Atomics.store(sync, 1, 2);
        Atomics.notify(sync, 1, 1);
      }
    }
  };
})();
