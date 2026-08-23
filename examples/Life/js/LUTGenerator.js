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

  /** Exact 8-cell Conway step for aligned bytes (top/cur/bot rows). */
  function computeNextByte(top, cur, bot, leftBit, rightBit) {
    let out = 0;
    for (let bit = 0; bit < 8; bit++) {
      const alive = (cur >> bit) & 1;
      let n = 0;
      const left = bit > 0 ? (cur >> (bit - 1)) & 1 : leftBit;
      const right = bit < 7 ? (cur >> (bit + 1)) & 1 : rightBit;
      const topL = bit > 0 ? (top >> (bit - 1)) & 1 : leftBit;
      const topC = (top >> bit) & 1;
      const topR = bit < 7 ? (top >> (bit + 1)) & 1 : rightBit;
      const botL = bit > 0 ? (bot >> (bit - 1)) & 1 : leftBit;
      const botC = (bot >> bit) & 1;
      const botR = bit < 7 ? (bot >> (bit + 1)) & 1 : rightBit;
      n = topL + topC + topR + left + right + botL + botC + botR;
      const next = alive ? n === 2 || n === 3 : n === 3;
      if (next) out |= 1 << bit;
    }
    return out;
  }

  /**
   * Build the 65536-entry LUT (bot row assumed dead; edge bits use 0 spill).
   */
  function createLifeLUT() {
    const LUT = new Uint8Array(65536);
    for (let key = 0; key < 65536; key++) {
      const top = (key >> 8) & 0xff;
      const cur = key & 0xff;
      LUT[key] = computeNextByte(top, cur, 0, 0, 0);
    }
    return LUT;
  }

  function stepChunk(LUT, top, cur, bot, leftBit, rightBit) {
    if ((top | cur | bot) === 0 && !leftBit && !rightBit) return 0;
    if (bot === 0 && !leftBit && !rightBit) return LUT[(top << 8) | cur];
    return computeNextByte(top, cur, bot, leftBit, rightBit);
  }

  /** Step rows [y0, y1) with LUT chunking. */
  function stepRegionLUT(LUT, gridA, gridB, w, h, y0, y1) {
    const yStart = Math.max(1, y0);
    const yStop = Math.min(h - 1, y1);
    for (let y = yStart; y < yStop; y++) {
      const topOff = (y - 1) * w;
      const curOff = y * w;
      const botOff = (y + 1) * w;
      const bytes = ((w + 7) / 8) | 0;
      for (let xb = 0; xb < bytes; xb++) {
        const x0 = xb * 8;
        if (x0 >= w) continue;
        const leftBit = x0 > 0 && (gridA[curOff + x0 - 1] & 1) ? 1 : 0;
        const rightCol = x0 + 8;
        const rightBit =
          rightCol < w && (gridA[curOff + rightCol] & 1) ? 1 : 0;
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
        if ((top | cur | bot) === 0 && !leftBit && !rightBit) continue;
        const next = stepChunk(LUT, top, cur, bot, leftBit, rightBit);
        for (let b = 0; b < 8; b++) {
          const x = x0 + b;
          if (x >= w) continue;
          gridB[curOff + x] = next & (1 << b) ? 1 : 0;
        }
      }
    }
    if (y0 === 0) gridB.set(gridA.subarray(0, w));
    if (y1 >= h) gridB.set(gridA.subarray((h - 1) * w, h * w), (h - 1) * w);
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
