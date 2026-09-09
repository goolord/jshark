// Measures the HVM2 parallel steal path on real threads.
//
// Mirrors examples/static/hvm2-grid-worker.js: eval workers are instantiated
// first over a shared memory, the coordinator last, and spawn_eval/wait_evals
// hand work to the pool. Reports ms per grid job for a given thread count.
//
//   LIVE=4 CELLS=4096 node wasm/hvm2/check-threads.mjs
import { readFileSync } from 'node:fs';
import { Worker } from 'node:worker_threads';
import { importedMemoryLimits } from './wasm-mem.mjs';

const LIVE = Number(process.env.LIVE ?? 4);
const CELLS = Number(process.env.CELLS ?? 4096);
const REPS = Number(process.env.REPS ?? 5);
const BX = Number(process.env.BX ?? 320);
const BY = Number(process.env.BY ?? 240);

const bytes = readFileSync(
  new URL('../../examples/static/hvm2-demo.wasm', import.meta.url),
);
const mod = new WebAssembly.Module(bytes);
const lim = importedMemoryLimits(bytes);
const memory = new WebAssembly.Memory({
  initial: lim.initial,
  maximum: lim.maximum,
  shared: true,
});
const evalSync = new Int32Array(new SharedArrayBuffer(16));
const wasi = new Proxy({}, { get: () => () => 0 });

const workers = [];
async function spawnPool(n) {
  for (let tid = 1; tid < n; tid++) {
    const w = new Worker(new URL('./thread-eval-worker.mjs', import.meta.url), {
      workerData: { module: mod, memory, evalSync, tid },
    });
    await new Promise((res) => w.once('message', res));
    workers.push({ tid, w });
  }
}

await spawnPool(LIVE);

let netPtr = 0;
let bookPtr = 0;

const imports = {};
for (const imp of WebAssembly.Module.imports(mod)) {
  imports[imp.module] ??= {};
  if (imp.kind === 'memory') {
    imports[imp.module][imp.name] = memory;
  } else if (imp.kind === 'function') {
    if (imp.module === 'wasi_snapshot_preview1') {
      imports[imp.module][imp.name] = wasi[imp.name];
    } else if (imp.name === 'live_threads') {
      imports[imp.module][imp.name] = () => LIVE;
    } else if (imp.name === 'reset_evals') {
      imports[imp.module][imp.name] = () => {
        Atomics.store(evalSync, 0, 0);
        Atomics.store(evalSync, 1, 0);
      };
    } else if (imp.name === 'spawn_eval') {
      imports[imp.module][imp.name] = (tid, np, bp) => {
        netPtr = np >>> 0;
        bookPtr = bp >>> 0;
        const slot = workers.find((x) => x.tid === tid);
        if (slot) {
          slot.w.postMessage({ type: 'eval', tid, netPtr, bookPtr });
        }
      };
    } else if (imp.name === 'wait_evals') {
      imports[imp.module][imp.name] = (count) => {
        const deadline = Date.now() + 15000;
        while (Atomics.load(evalSync, 0) < count) {
          if (Date.now() > deadline) {
            throw new Error('wait_evals timeout');
          }
          Atomics.wait(evalSync, 0, Atomics.load(evalSync, 0), 50);
        }
      };
    } else {
      imports[imp.module][imp.name] = () => 0;
    }
  }
}

const coord = new WebAssembly.Instance(mod, imports).exports;

const times = [];
let lastK = 0;
let ptr = 0;
for (let i = 0; i < REPS; i++) {
  coord.jshark_set_grid_cap(CELLS);
  coord.jshark_fail_reset();
  const t0 = performance.now();
  // Nudge the camera so each rep is a distinct job.
  ptr = coord.mandel_hvm2_grid(-0.5 + i * 1e-4, 0, 3.0, 320, 240, 20, BX, BY);
  times.push(performance.now() - t0);
  lastK = coord.jshark_hvm2_last_k();
}

const best = Math.min(...times);
const med = times.slice().sort((a, b) => a - b)[times.length >> 1];
console.log(
  `LIVE=${LIVE} CELLS=${CELLS} ptr=${ptr >>> 0} lastK=${lastK} ` +
    `best=${best.toFixed(1)}ms med=${med.toFixed(1)}ms ` +
    `failHits=${coord.jshark_fail_hits()} failSpin=${coord.jshark_fail_spin()} ` +
    `all=[${times.map((t) => t.toFixed(0)).join(',')}]`,
);

for (const { w } of workers) {
  w.postMessage({ type: 'die' });
  await w.terminate();
}
