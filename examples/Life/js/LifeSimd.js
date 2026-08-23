'use strict';

/**
 * Optional WASM SIMD helpers for LifeLUT row clears/copies.
 * Grids must live in the wasm module's exported memory (see LifeEngine.loadWasm).
 */
(function (global) {
  const LifeSimd = {
    ready: false,
    memory: null,
    growTo: null,
  };

  let wasmClearRow = null;
  let wasmCopyRow = null;

  function gridInWasmMemory(grid) {
    return (
      LifeSimd.ready &&
      LifeSimd.memory &&
      grid &&
      grid.buffer === LifeSimd.memory.buffer
    );
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
        typeof exp.copyRow !== 'function'
      ) {
        return false;
      }
      const n = (width | 0) * (height | 0);
      if (exp.growTo(n * 2) !== 0) return false;
      LifeSimd.memory = exp.memory;
      LifeSimd.growTo = exp.growTo;
      wasmClearRow = exp.clearRow;
      wasmCopyRow = exp.copyRow;
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
  global.LifeSimd = LifeSimd;
})(typeof self !== 'undefined' ? self : globalThis);
