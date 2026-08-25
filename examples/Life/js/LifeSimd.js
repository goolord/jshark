'use strict';

/**
 * WASM SIMD helpers for LifeLUT: row clears/copies and stepRegionLUT.
 * Grids must live in the wasm module's exported memory (see LifeEngine.loadWasm).
 */
(function (global) {
  const LifeSimd = {
    ready: false,
    memory: null,
    growTo: null,
    lutOffset: 0,
  };

  let wasmClearRow = null;
  let wasmCopyRow = null;
  let wasmStepRegionLUT = null;

  function gridInWasmMemory(grid) {
    return (
      LifeSimd.ready &&
      LifeSimd.memory &&
      grid &&
      grid.buffer === LifeSimd.memory.buffer
    );
  }

  /**
   * Step via wasm when grids live in linear memory. Returns true if handled.
   */
  function stepRegionLUT(_LUT, gridA, gridB, w, h, y0, y1) {
    if (
      !wasmStepRegionLUT ||
      !gridInWasmMemory(gridA) ||
      !gridInWasmMemory(gridB)
    ) {
      return false;
    }
    wasmStepRegionLUT(
      LifeSimd.lutOffset,
      gridA.byteOffset,
      gridB.byteOffset,
      w | 0,
      h | 0,
      y0 | 0,
      y1 | 0
    );
    return true;
  }

  function clearRow(grid, off, len) {
    if (gridInWasmMemory(grid)) {
      wasmClearRow(off + grid.byteOffset, len);
      return;
    }
    grid.fill(0, off, off + len);
  }

  function copyRow(src, srcOff, dst, dstOff, len) {
    if (gridInWasmMemory(src) && gridInWasmMemory(dst)) {
      wasmCopyRow(
        srcOff + src.byteOffset,
        dstOff + dst.byteOffset,
        len
      );
      return;
    }
    dst.set(src.subarray(srcOff, srcOff + len), dstOff);
  }

  async function load(url, width, height) {
    if (!global.WebAssembly) return false;
    try {
      const resp = await fetch(url);
      if (!resp.ok) return false;
      const bytes = await resp.arrayBuffer();
      const { instance } = await WebAssembly.instantiate(bytes, {});
      const exp = instance.exports;
      if (
        !exp.memory ||
        typeof exp.growTo !== 'function' ||
        typeof exp.clearRow !== 'function' ||
        typeof exp.copyRow !== 'function' ||
        typeof exp.initLUT !== 'function' ||
        typeof exp.stepRegionLUT !== 'function'
      ) {
        return false;
      }
      const n = (width | 0) * (height | 0);
      const lutBytes = 65536;
      LifeSimd.lutOffset = n * 2;
      if (exp.growTo(n * 2 + lutBytes) !== 0) return false;
      exp.initLUT(LifeSimd.lutOffset);
      LifeSimd.memory = exp.memory;
      LifeSimd.growTo = exp.growTo;
      wasmClearRow = exp.clearRow;
      wasmCopyRow = exp.copyRow;
      wasmStepRegionLUT = exp.stepRegionLUT;
      LifeSimd.ready = true;
      return true;
    } catch (_) {
      /* no wasm bundle shipped */
    }
    return false;
  }

  function bindGrids(engine) {
    if (!LifeSimd.ready || !engine || !engine.gridA || !engine.gridB) return false;
    if (engine.mode === 'workers') return false;
    const n = engine.width * engine.height;
    const mem = LifeSimd.memory.buffer;
    if (engine.gridA.buffer === mem && engine.gridB.buffer === mem) return true;
    const gridA = new Uint8Array(mem, 0, n);
    const gridB = new Uint8Array(mem, n, n);
    gridA.set(engine.gridA);
    gridB.set(engine.gridB);
    engine.gridA = gridA;
    engine.gridB = gridB;
    return true;
  }

  LifeSimd.load = load;
  LifeSimd.bindGrids = bindGrids;
  LifeSimd.clearRow = clearRow;
  LifeSimd.copyRow = copyRow;
  LifeSimd.stepRegionLUT = stepRegionLUT;
  global.LifeSimd = LifeSimd;
})(typeof self !== 'undefined' ? self : globalThis);
