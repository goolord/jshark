'use strict';

/**
 * Static lookup tables and chunk stepping for 8-cell horizontal bytes.
 * Exposed on globalThis.LifeLUT for importScripts in workers.
 */
(function (global) {
  function countNeighbors(grid, w, h, x, y) {
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
    return n;
  }

  function stepCell(gridA, gridB, w, h, x, y) {
    const i = y * w + x;
    const alive = gridA[i] & 1;
    const n = countNeighbors(gridA, w, h, x, y);
    const next = alive ? n === 2 || n === 3 : n === 3;
    gridB[i] = next ? 1 : 0;
  }

  /**
   * Exact 8-cell Conway step for aligned bytes.
   * Edge bits are per-row: lt/lc/lb are the cell just left of the byte on
   * top/cur/bot; rt/rc/rb are the cell just right. Reusing the current-row
   * edge for diagonals miscounts neighbors on 8-cell seams.
   */
  function computeNextByte(top, cur, bot, lt, lc, lb, rt, rc, rb) {
    let out = 0;
    for (let bit = 0; bit < 8; bit++) {
      const alive = (cur >> bit) & 1;
      const left = bit > 0 ? (cur >> (bit - 1)) & 1 : lc;
      const right = bit < 7 ? (cur >> (bit + 1)) & 1 : rc;
      const topL = bit > 0 ? (top >> (bit - 1)) & 1 : lt;
      const topC = (top >> bit) & 1;
      const topR = bit < 7 ? (top >> (bit + 1)) & 1 : rt;
      const botL = bit > 0 ? (bot >> (bit - 1)) & 1 : lb;
      const botC = (bot >> bit) & 1;
      const botR = bit < 7 ? (bot >> (bit + 1)) & 1 : rb;
      const n = topL + topC + topR + left + right + botL + botC + botR;
      const next = alive ? n === 2 || n === 3 : n === 3;
      if (next) out |= 1 << bit;
    }
    return out;
  }

  /**
   * Build the 65536-entry LUT (bot row assumed dead; all six edge bits 0).
   */
  function createLifeLUT() {
    const LUT = new Uint8Array(65536);
    for (let key = 0; key < 65536; key++) {
      const top = (key >> 8) & 0xff;
      const cur = key & 0xff;
      LUT[key] = computeNextByte(top, cur, 0, 0, 0, 0, 0, 0, 0);
    }
    return LUT;
  }

  function stepChunk(LUT, top, cur, bot, lt, lc, lb, rt, rc, rb) {
    const edge = lt | lc | lb | rt | rc | rb;
    if ((top | cur | bot | edge) === 0) return 0;
    if (bot === 0 && edge === 0) return LUT[(top << 8) | cur];
    return computeNextByte(top, cur, bot, lt, lc, lb, rt, rc, rb);
  }

  /** Step rows [y0, y1) with LUT chunking. */
  function stepRegionLUT(LUT, gridA, gridB, w, h, y0, y1) {
    const simd = global.LifeSimd;
    const yStart = Math.max(1, y0);
    const yStop = Math.min(h - 1, y1);
    for (let y = yStart; y < yStop; y++) {
      const topOff = (y - 1) * w;
      const curOff = y * w;
      const botOff = (y + 1) * w;
      // Ping-pong reuses gridB as the previous generation. Empty chunks
      // must not leave those stale live bits behind (glider-on-seam ghosts).
      if (simd) simd.clearRow(gridB, curOff, w);
      else gridB.fill(0, curOff, curOff + w);
      const bytes = ((w + 7) / 8) | 0;
      for (let xb = 0; xb < bytes; xb++) {
        const x0 = xb * 8;
        if (x0 >= w) continue;
        const leftCol = x0 - 1;
        const rightCol = x0 + 8;
        const lt = x0 > 0 && (gridA[topOff + leftCol] & 1) ? 1 : 0;
        const lc = x0 > 0 && (gridA[curOff + leftCol] & 1) ? 1 : 0;
        const lb = x0 > 0 && (gridA[botOff + leftCol] & 1) ? 1 : 0;
        const rt = rightCol < w && (gridA[topOff + rightCol] & 1) ? 1 : 0;
        const rc = rightCol < w && (gridA[curOff + rightCol] & 1) ? 1 : 0;
        const rb = rightCol < w && (gridA[botOff + rightCol] & 1) ? 1 : 0;
        let top = 0;
        let cur = 0;
        let bot = 0;
        for (let b = 0; b < 8; b++) {
          const x = x0 + b;
          if (x >= w) continue;
          const sh = 1 << b;
          if (gridA[topOff + x] & 1) top |= sh;
          if (gridA[curOff + x] & 1) cur |= sh;
          if (gridA[botOff + x] & 1) bot |= sh;
        }
        if ((top | cur | bot | lt | lc | lb | rt | rc | rb) === 0) continue;
        const next = stepChunk(LUT, top, cur, bot, lt, lc, lb, rt, rc, rb);
        for (let b = 0; b < 8; b++) {
          const x = x0 + b;
          if (x >= w) continue;
          gridB[curOff + x] = next & (1 << b) ? 1 : 0;
        }
      }
    }
    if (y0 === 0) {
      if (simd) simd.copyRow(gridA, 0, gridB, 0, w);
      else gridB.set(gridA.subarray(0, w));
    }
    if (y1 >= h) {
      const botOff = (h - 1) * w;
      if (simd) simd.copyRow(gridA, botOff, gridB, botOff, w);
      else gridB.set(gridA.subarray(botOff, h * w), botOff);
    }
  }

  global.LifeLUT = {
    createLifeLUT,
    computeNextByte,
    stepChunk,
    stepCell,
    countNeighbors,
    stepRegionLUT,
  };
})(typeof self !== 'undefined' ? self : globalThis);
