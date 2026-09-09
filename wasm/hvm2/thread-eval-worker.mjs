// Eval worker for check-threads.mjs. Mirrors examples/static/hvm2-worker.js
// but on node:worker_threads, so the parallel steal path runs on real threads.
import { parentPort, workerData } from 'node:worker_threads';

const { module: mod, memory, evalSync, tid } = workerData;
const wasi = new Proxy({}, { get: () => () => 0 });

const imports = {};
for (const imp of WebAssembly.Module.imports(mod)) {
  imports[imp.module] ??= {};
  if (imp.kind === 'memory') {
    imports[imp.module][imp.name] = memory;
  } else if (imp.kind === 'function') {
    if (imp.module === 'wasi_snapshot_preview1') {
      imports[imp.module][imp.name] = wasi[imp.name];
    } else if (imp.name === 'eval_done') {
      imports[imp.module][imp.name] = () => {
        Atomics.add(evalSync, 0, 1);
        Atomics.notify(evalSync, 0);
      };
    } else {
      imports[imp.module][imp.name] = () => 0;
    }
  }
}

const ex = new WebAssembly.Instance(mod, imports).exports;
parentPort.postMessage({ type: 'ready', tid });

parentPort.on('message', (msg) => {
  if (msg.type === 'eval') {
    ex.jshark_worker_eval(msg.tid, msg.netPtr, msg.bookPtr);
  } else if (msg.type === 'die') {
    process.exit(0);
  }
});
