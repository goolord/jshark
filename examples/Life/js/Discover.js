/**
 * Species discovery: empirical phase collection + shape keys.
 * Exposes globalThis.LifeDiscover.
 */
(function (global) {
  'use strict';

  function normCells(coords) {
    if (!coords.length) return '';
    let minX = 1e9;
    let minY = 1e9;
    for (let i = 0; i < coords.length; i++) {
      const x = coords[i][0];
      const y = coords[i][1];
      if (x < minX) minX = x;
      if (y < minY) minY = y;
    }
    const pts = new Array(coords.length);
    for (let i = 0; i < coords.length; i++) {
      pts[i] = [coords[i][0] - minX, coords[i][1] - minY];
    }
    pts.sort((a, b) => a[0] - b[0] || a[1] - b[1]);
    let out = pts[0][0] + ',' + pts[0][1];
    for (let i = 1; i < pts.length; i++) {
      out += ';' + pts[i][0] + ',' + pts[i][1];
    }
    return out;
  }

  const rots = [
    (x, y) => [x, y],
    (x, y) => [-y, x],
    (x, y) => [-x, -y],
    (x, y) => [y, -x],
  ];

  function canonicalHash(coords) {
    let best = null;
    for (let ri = 0; ri < rots.length; ri++) {
      const r = rots[ri];
      for (let flip = 0; flip < 2; flip++) {
        const pts = new Array(coords.length);
        for (let i = 0; i < coords.length; i++) {
          let x = coords[i][0];
          let y = coords[i][1];
          let a = r(x, y)[0];
          let b = r(x, y)[1];
          if (flip) a = -a;
          pts[i] = [a, b];
        }
        const h = normCells(pts);
        if (best === null || h < best) best = h;
      }
    }
    return best || '';
  }

  function extractCoords(w, cells) {
    const n = cells.length | 0;
    const coords = new Array(n);
    for (let k = 0; k < n; k++) {
      const i = cells[k] | 0;
      coords[k] = [i % w, (i / w) | 0];
    }
    return coords;
  }

  function stepGrid(grid, gw, gh) {
    const out = new Uint8Array(gw * gh);
    for (let y = 0; y < gh; y++) {
      const row = y * gw;
      for (let x = 0; x < gw; x++) {
        let n = 0;
        for (let dy = -1; dy <= 1; dy++) {
          for (let dx = -1; dx <= 1; dx++) {
            if (!dx && !dy) continue;
            const nx = x + dx;
            const ny = y + dy;
            if (nx < 0 || ny < 0 || nx >= gw || ny >= gh) continue;
            if (grid[ny * gw + nx]) n++;
          }
        }
        const alive = grid[row + x];
        out[row + x] = n === 3 || (alive && n === 2) ? 1 : 0;
      }
    }
    return out;
  }

  function centroid(coords) {
    let cx = 0;
    let cy = 0;
    for (let i = 0; i < coords.length; i++) {
      cx += coords[i][0];
      cy += coords[i][1];
    }
    const inv = 1 / coords.length;
    return [cx * inv, cy * inv];
  }

  function collectPhaseKey(coords) {
    if (!coords.length) return { key: '', hashes: [] };

    let minX = 1e9;
    let minY = 1e9;
    let maxX = -1e9;
    let maxY = -1e9;
    for (let i = 0; i < coords.length; i++) {
      const x = coords[i][0];
      const y = coords[i][1];
      if (x < minX) minX = x;
      if (y < minY) minY = y;
      if (x > maxX) maxX = x;
      if (y > maxY) maxY = y;
    }

    const pad = 2;
    const ox = minX - pad;
    const oy = minY - pad;
    const gw = maxX - minX + 1 + pad * 2;
    const gh = maxY - minY + 1 + pad * 2;
    let grid = new Uint8Array(gw * gh);
    for (let i = 0; i < coords.length; i++) {
      const x = coords[i][0] - ox;
      const y = coords[i][1] - oy;
      grid[y * gw + x] = 1;
    }

    const [c0x, c0y] = centroid(coords);
    const history = new Set();
    const hashes = [];
    const maxSteps = 32;

    for (let step = 0; step < maxSteps; step++) {
      const local = [];
      for (let y = 0; y < gh; y++) {
        const row = y * gw;
        for (let x = 0; x < gw; x++) {
          if (grid[row + x]) local.push([x, y]);
        }
      }
      if (!local.length) break;

      const exact = normCells(local);
      if (history.has(exact)) break;
      history.add(exact);
      hashes.push(exact);

      if (step > 0) {
        const abs = new Array(local.length);
        for (let i = 0; i < local.length; i++) {
          abs[i] = [local[i][0] + ox, local[i][1] + oy];
        }
        const [cx, cy] = centroid(abs);
        if (Math.abs(cx - c0x) + Math.abs(cy - c0y) > 0.75) break;
      }

      grid = stepGrid(grid, gw, gh);
    }

    let key;
    if (hashes.length > 1) {
      const sorted = hashes.slice().sort();
      key = sorted.join('|');
    } else if (hashes.length === 1) {
      key = canonicalHash(coords);
    } else {
      key = canonicalHash(coords);
    }

    return { key, hashes };
  }

  function discoverRgb(n) {
    const hue = ((n * 137.508) % 360 + 360) % 360;
    const s = 0.62;
    const l = 0.41;
    const c = (1 - Math.abs(2 * l - 1)) * s;
    const hp = hue / 60;
    const hpMod = hp - 2 * Math.floor(hp / 2);
    const x = c * (1 - Math.abs(hpMod - 1));
    const m = l - c / 2;
    let r1;
    let g1;
    let b1;
    if (hp < 1) {
      r1 = c;
      g1 = x;
      b1 = 0;
    } else if (hp < 2) {
      r1 = x;
      g1 = c;
      b1 = 0;
    } else if (hp < 3) {
      r1 = 0;
      g1 = c;
      b1 = x;
    } else if (hp < 4) {
      r1 = 0;
      g1 = x;
      b1 = c;
    } else if (hp < 5) {
      r1 = x;
      g1 = 0;
      b1 = c;
    } else {
      r1 = c;
      g1 = 0;
      b1 = x;
    }
    const clamp = (t) =>
      Math.max(0, Math.min(255, Math.round(255 * (t + m))));
    return [clamp(r1), clamp(g1), clamp(b1)];
  }

  function registerAliases(seen, hashes, sid) {
    for (let k = 0; k < hashes.length; k++) {
      seen[hashes[k]] = sid;
    }
  }

  function resolveSpecies(registry, key, hashes, nextId, maxSid) {
    const known = registry.known;
    const seen = registry.seen;
    let pending = registry.pending;
    if (!pending) {
      pending = {};
      registry.pending = pending;
    }

    let sid = known[key];
    if (sid !== undefined) {
      seen[key] = sid;
      registerAliases(seen, hashes, sid);
      return { action: 1, sid };
    }

    sid = seen[key];
    if (sid !== undefined) {
      registerAliases(seen, hashes, sid);
      return { action: 1, sid };
    }

    for (let k = 0; k < hashes.length; k++) {
      sid = seen[hashes[k]];
      if (sid !== undefined) {
        seen[key] = sid;
        registerAliases(seen, hashes, sid);
        return { action: 1, sid };
      }
    }

    const cnt = (pending[key] | 0) + 1;
    pending[key] = cnt;
    if (cnt < 2) return { action: 0, sid: 0 };

    if (nextId > maxSid) return { action: 0, sid: 0 };

    const nid = nextId | 0;
    const rgb = discoverRgb(nid);
    seen[key] = nid;
    registerAliases(seen, hashes, nid);
    delete pending[key];
    return { action: 2, sid: nid, r: rgb[0], g: rgb[1], b: rgb[2] };
  }

  function classifyAndResolveImpl(registry, alive, w, cells, nextId, maxSid) {
    const coords = extractCoords(w | 0, cells);
    const info = collectPhaseKey(coords);
    if (!info.key) return { action: 0, sid: 0 };
    const out = resolveSpecies(
      registry,
      info.key,
      info.hashes,
      nextId,
      maxSid
    );
    out.key = info.key;
    return out;
  }

  global.LifeDiscover = {
    classify(_alive, w, cells) {
      const coords = extractCoords(w | 0, cells);
      return collectPhaseKey(coords);
    },
    classifyAndResolve(registry, alive, w, cells, nextId, maxSid) {
      return classifyAndResolveImpl(
        registry,
        alive,
        w,
        cells,
        nextId,
        maxSid
      );
    },
  };
})(globalThis);
