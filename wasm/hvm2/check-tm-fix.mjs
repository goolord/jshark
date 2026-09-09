// Headless check for the eval-worker TM binding fix.
//
// Instantiates the demo wasm twice over one shared memory: a "coordinator"
// (which boots) and a "worker" (which never boots, like hvm2-worker.js).
// Before the fix the worker saw tm[tid] == NULL; after it both instances
// must agree on the same non-null tm[] slots.
import { readFileSync } from 'node:fs';
import { importedMemoryLimits } from './wasm-mem.mjs';

const LIVE = Number(process.env.LIVE ?? 4);
const CELLS = Number(process.env.CELLS ?? 0);

const bytes = readFileSync(
  new URL('../../examples/static/hvm2-demo.wasm', import.meta.url),
);
const module = new WebAssembly.Module(bytes);
const lim = importedMemoryLimits(bytes);
const memory = new WebAssembly.Memory({
  initial: lim.initial,
  maximum: lim.maximum,
  shared: true,
});

const wasi = new Proxy({}, { get: () => () => 0 });

function instantiate(label, spawnEval) {
  const imports = {};
  for (const imp of WebAssembly.Module.imports(module)) {
    imports[imp.module] ??= {};
    if (imp.kind === 'memory') {
      imports[imp.module][imp.name] = memory;
    } else if (imp.kind === 'function') {
      if (imp.module === 'wasi_snapshot_preview1') {
        imports[imp.module][imp.name] = wasi[imp.name];
      } else if (imp.name === 'spawn_eval') {
        imports[imp.module][imp.name] = spawnEval ?? (() => {});
      } else if (imp.name === 'live_threads') {
        imports[imp.module][imp.name] = () => LIVE;
      } else {
        imports[imp.module][imp.name] = () => 0;
      }
    }
  }
  return new WebAssembly.Instance(module, imports).exports;
}

const tms = (ex) =>
  [0, 1, 2, 3].map((i) => ({
    addr: ex.jshark_tm_addr(i),
    tid: ex.jshark_tm_state(i, 0) | 0,
  }));

// Workers are instantiated first, coordinator last (as the grid worker does).
const worker = instantiate('worker');
const coord = instantiate('coord', (tid, netPtr, bookPtr) => {
  // Emulate the eval worker: it never boots, it only calls worker_eval.
  worker.jshark_worker_eval(tid, netPtr, bookPtr);
});

console.log('worker tm before any boot:', JSON.stringify(tms(worker)));

if (CELLS > 0 && typeof coord.jshark_set_grid_cap === 'function') {
  coord.jshark_set_grid_cap(CELLS);
}
const BX = Number(process.env.BX ?? 16);
const BY = Number(process.env.BY ?? 12);
const t0 = performance.now();
const ptr = coord.mandel_hvm2_grid(-0.5, 0, 3.0, 320, 240, 20, BX, BY);
const ms = performance.now() - t0;
const lastK = coord.jshark_hvm2_last_k();
console.log(`TIMING cells=${CELLS || 'default'} bx=${BX} by=${BY} ms=${ms.toFixed(1)} ptr=${ptr >>> 0} lastK=${lastK}`);

console.log('coord  tm after boot:     ', JSON.stringify(tms(coord)));
console.log('worker tm after boot:     ', JSON.stringify(tms(worker)));
console.log('netAddr coord/worker:     ',
  coord.jshark_net_addr(), worker.jshark_net_addr());
console.log('grid ptr:', ptr >>> 0, 'lastK:', lastK,
  'failHits:', coord.jshark_fail_hits(),
  'failSpin:', coord.jshark_fail_spin());

if (ptr) {
  const grid = new Int32Array(memory.buffer, ptr, 16 * 12);
  let mn = Infinity;
  let mx = -Infinity;
  for (const v of grid) {
    if (v < mn) mn = v;
    if (v > mx) mx = v;
  }
  console.log('grid min/max:', mn, mx, 'first row:',
    Array.from(grid.slice(0, 16)).join(','));
}
