'use strict';

/**
 * HVM2 evaluator worker: shares the main thread's wasm Memory (SharedArrayBuffer)
 * and runs jshark_worker_eval(tid, netPtr, bookPtr) on demand.
 */
const g = globalThis;

const EVAL_DONE = 0;
const EVAL_ERR = 1;

const wasi = {
  args_get: () => 0,
  args_sizes_get: () => 0,
  fd_fdstat_get: () => 0,
  fd_seek: () => 0,
  fd_write: () => 0,
  fd_close: () => 0,
  sched_yield: () => 0,
  proc_exit: () => {},
  clock_time_get: () => 0,
  random_get: () => 0,
  environ_get: () => 0,
  environ_sizes_get: () => 0,
};

/** @param {WebAssembly.Module} module @param {Int32Array} evalSync @param {WebAssembly.Memory} memory */
function buildImports(module, evalSync, memory) {
  /** @type {WebAssembly.Imports} */
  const out = {};
  for (const imp of WebAssembly.Module.imports(module)) {
    if (!out[imp.module]) {
      out[imp.module] = {};
    }
    const ns = out[imp.module];
    if (imp.kind === 'function') {
      if (imp.module === 'wasi_snapshot_preview1') {
        ns[imp.name] = wasi[imp.name] ?? (() => 0);
      } else if (imp.module === 'jshark') {
        if (imp.name === 'eval_done') {
          ns[imp.name] = () => {
            Atomics.add(evalSync, EVAL_DONE, 1);
            Atomics.notify(evalSync, EVAL_DONE);
          };
        } else {
          ns[imp.name] = () => {};
        }
      }
    } else if (imp.kind === 'memory') {
      if (!memory) {
        throw new Error('worker init missing shared memory for env import');
      }
      ns[imp.name] = memory;
    }
  }
  return out;
}

let instance = null;

function signalError(evalSync) {
  Atomics.store(evalSync, EVAL_ERR, 1);
  Atomics.add(evalSync, EVAL_DONE, 1);
  Atomics.notify(evalSync, EVAL_DONE);
}

function fail(workerId, evalSync, err) {
  if (evalSync) {
    signalError(evalSync);
  }
  g.postMessage({
    type: 'error',
    workerId,
    message: err && err.message ? err.message : String(err),
  });
}

g.onmessage = (ev) => {
  const msg = ev.data;
  if (msg.type === 'init') {
    try {
      if (!msg.memory) {
        throw new Error('worker init missing shared WebAssembly.Memory');
      }
      if (!msg.evalSync || !(msg.evalSync.buffer instanceof SharedArrayBuffer)) {
        throw new Error('worker init missing evalSync SharedArrayBuffer');
      }
      let module = msg.module;
      if (!module) {
        if (!msg.wasmBytes) {
          throw new Error('worker init missing WebAssembly.Module or wasm bytes');
        }
        module = new WebAssembly.Module(msg.wasmBytes);
      }
      instance = new WebAssembly.Instance(
        module,
        buildImports(module, msg.evalSync, msg.memory),
      );
      if (typeof instance.exports.jshark_worker_eval !== 'function') {
        throw new Error('worker wasm missing jshark_worker_eval export');
      }
      g.postMessage({ type: 'ready', workerId: msg.workerId });
    } catch (err) {
      fail(msg.workerId, msg.evalSync, err);
    }
    return;
  }
  if (msg.type === 'eval') {
    try {
      if (!instance) {
        throw new Error('worker eval before init');
      }
      instance.exports.jshark_worker_eval(msg.tid, msg.netPtr, msg.bookPtr);
    } catch (err) {
      fail(msg.workerId, msg.evalSync, err);
    }
  }
};
