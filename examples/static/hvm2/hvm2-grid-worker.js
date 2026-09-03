'use strict';

/**
 * Dedicated HVM2 grid worker: own wasm instance + eval pool.
 * Coordinator runs mandel_hvm2_grid; nested hvm2-worker.js threads
 * steal redexes so the UI thread never enters normalize.
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

function importedMemoryPages(wasmBytes) {
  const u8 =
    wasmBytes instanceof Uint8Array
      ? wasmBytes
      : new Uint8Array(wasmBytes);
  const view = new DataView(u8.buffer, u8.byteOffset, u8.byteLength);
  let pos = 8;
  while (pos < view.byteLength) {
    const sectionId = view.getUint8(pos++);
    const sectionSize = readVarUint(view, pos);
    pos = sectionSize.next;
    const end = pos + sectionSize.value;
    if (sectionId === 2) {
      let p = pos;
      const count = readVarUint(view, p);
      p = count.next;
      for (let i = 0; i < count.value; i++) {
        const mod = readName(view, p);
        p = mod.next;
        const name = readName(view, p);
        p = name.next;
        const kind = view.getUint8(p++);
        if (kind === 0x02) {
          const flags = view.getUint8(p++);
          const initial = readVarUint(view, p);
          p = initial.next;
          let maximum = initial.value;
          if (flags & 1) {
            const max = readVarUint(view, p);
            maximum = max.value;
          }
          return {
            initial: initial.value,
            maximum: maximum,
            shared: !!(flags & 2),
          };
        }
        if (kind === 0x00) p++;
        else if (kind === 0x01) {
          const flags = view.getUint8(p++);
          const min = readVarUint(view, p);
          p = min.next;
          if (flags & 1) readVarUint(view, p);
        } else if (kind === 0x03) p += 2;
        else if (kind === 0x04) p += 2;
      }
    }
    pos = end;
  }
  return { initial: 256, maximum: 256, shared: false };
}

function readVarUint(view, pos) {
  let result = 0;
  let shift = 0;
  let byte = 0;
  do {
    byte = view.getUint8(pos++);
    result |= (byte & 0x7f) << shift;
    shift += 7;
  } while (byte & 0x80);
  return { value: result, next: pos };
}

function readName(view, pos) {
  const len = readVarUint(view, pos);
  pos = len.next;
  pos += len.value;
  return { next: pos };
}

function resetEvals(evalSync) {
  Atomics.store(evalSync, EVAL_DONE, 0);
  Atomics.store(evalSync, EVAL_ERR, 0);
}

function waitEvals(evalSync, count, onTimeout) {
  const deadline = performance.now() + 60000;
  for (;;) {
    const done = Atomics.load(evalSync, EVAL_DONE);
    if (Atomics.load(evalSync, EVAL_ERR) !== 0) {
      throw new Error('HVM2 worker eval failed');
    }
    if (done >= count) {
      return;
    }
    const ms = deadline - performance.now();
    if (ms <= 0) {
      if (typeof onTimeout === 'function') {
        onTimeout();
      }
      throw new Error('HVM2 worker wait timeout');
    }
    Atomics.wait(evalSync, EVAL_DONE, done, ms);
  }
}

/** @param {WebAssembly.Module} module @param {WebAssembly.Memory} memory */
function buildImports(module, memory, jshark) {
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
        ns[imp.name] = jshark[imp.name] ?? (() => {});
      }
    } else if (imp.kind === 'memory') {
      ns[imp.name] = memory;
    }
  }
  return out;
}

/** @type {WebAssembly.Exports | null} */
let ex = null;
/** @type {WebAssembly.Memory | null} */
let memory = null;
/** @type {Worker[]} */
const evalWorkers = [];
let live = 1;
/** @type {Int32Array | null} */
let evalSync = null;

function killPool() {
  if (typeof ex?.jshark_cancel_eval === 'function') {
    try {
      ex.jshark_cancel_eval();
    } catch (_) {
      /* ignore */
    }
  }
  if (evalSync) {
    Atomics.store(evalSync, EVAL_ERR, 1);
    Atomics.notify(evalSync, EVAL_DONE);
  }
  for (const w of evalWorkers) {
    try {
      w.terminate();
    } catch (_) {
      /* ignore */
    }
  }
  evalWorkers.length = 0;
  live = 1;
}

function fail(jobId, kind, err, epoch) {
  g.postMessage({
    type: 'error',
    jobId,
    epoch: epoch | 0,
    kind: kind || 'trap',
    message: err && err.message ? err.message : String(err),
  });
}

function spawnEvalPool(module, wasmBytes, workerUrl, tpc, canEval) {
  const hw =
    (g.navigator && g.navigator.hardwareConcurrency) || tpc;
  const want = Math.max(1, Math.min(tpc, hw | 0));
  if (
    want <= 1 ||
    !workerUrl ||
    !evalSync ||
    !canEval ||
    !(memory && memory.buffer instanceof SharedArrayBuffer)
  ) {
    live = 1;
    return Promise.resolve();
  }
  const pool = want - 1;
  return new Promise((resolve) => {
    let settled = false;
    let timer = 0;
    const finishOnce = (ok) => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timer);
      if (!ok) {
        killPool();
      } else {
        live = want;
      }
      resolve();
    };
    timer = setTimeout(() => {
      console.warn('HVM2 eval pool init timeout; single-thread grid');
      finishOnce(false);
    }, 30000);
    let ready = 0;
    try {
      for (let i = 0; i < pool; i++) {
        const w = new Worker(workerUrl);
        let initAttempt = 0;
        const sendInit = (useModule) => {
          w.postMessage({
            type: 'init',
            module: useModule ? module : null,
            wasmBytes,
            memory,
            evalSync,
            workerId: i,
          });
        };
        w.onmessage = (ev) => {
          const m = ev.data;
          if (m.type === 'error') {
            finishOnce(false);
            return;
          }
          if (m.type === 'ready') {
            ready++;
            if (ready === pool) {
              finishOnce(true);
            }
          }
        };
        w.onerror = () => finishOnce(false);
        w.onmessageerror = () => {
          if (initAttempt === 0) {
            initAttempt = 1;
            sendInit(false);
          } else {
            finishOnce(false);
          }
        };
        evalWorkers[i] = w;
        sendInit(true);
      }
    } catch (_) {
      finishOnce(false);
    }
  });
}

async function initGrid(msg) {
  if (!msg.wasmBytes) {
    throw new Error('grid worker init missing wasm bytes');
  }
  const wasmBytes = msg.wasmBytes;
  const module = new WebAssembly.Module(wasmBytes);
  const memPages = importedMemoryPages(wasmBytes);
  memory = new WebAssembly.Memory({
    initial: memPages.initial,
    maximum: memPages.maximum,
    shared: memPages.shared,
  });
  if (memory.buffer instanceof SharedArrayBuffer) {
    evalSync = new Int32Array(new SharedArrayBuffer(16));
  }
  const jshark = {
    spawn_eval(tid, netPtr, bookPtr) {
      const w = evalWorkers[tid - 1];
      if (!w) {
        return;
      }
      w.postMessage({ type: 'eval', tid, netPtr, bookPtr });
    },
    wait_evals(count) {
      if (!evalSync) {
        return;
      }
      waitEvals(evalSync, count, killPool);
    },
    eval_done() {},
    reset_evals() {
      if (evalSync) {
        resetEvals(evalSync);
      }
    },
    live_threads() {
      return live;
    },
  };
  const stubs = {
    spawn_eval() {},
    wait_evals() {},
    eval_done() {},
    reset_evals() {},
    live_threads() {
      return 1;
    },
  };
  const probe = new WebAssembly.Instance(
    module,
    buildImports(module, memory, stubs),
  );
  const tpc =
    typeof probe.exports.jshark_tpc === 'function'
      ? probe.exports.jshark_tpc() >>> 0
      : 1;
  const canEval = typeof probe.exports.jshark_worker_eval === 'function';
  await spawnEvalPool(module, wasmBytes, msg.workerUrl, tpc, canEval);
  const instance = new WebAssembly.Instance(
    module,
    buildImports(module, memory, jshark),
  );
  ex = instance.exports;
  if (typeof ex.mandel_hvm2_grid !== 'function') {
    throw new Error('grid worker wasm missing mandel_hvm2_grid');
  }
}

function shutdown() {
  killPool();
  try {
    g.postMessage({ type: 'dead' });
  } catch (_) {
    /* ignore */
  }
  if (typeof g.close === 'function') {
    g.close();
  }
}

g.onmessage = (ev) => {
  const msg = ev.data;
  if (msg.type === 'die') {
    shutdown();
    return;
  }
  if (msg.type === 'init') {
    initGrid(msg)
      .then(() => {
        g.postMessage({ type: 'ready', threads: live });
      })
      .catch((err) => {
        fail(0, 'no-export', err);
      });
    return;
  }
  if (msg.type === 'grid') {
    const jobId = msg.jobId | 0;
    const epoch = msg.epoch | 0;
    try {
      if (!ex || !memory) {
        throw new Error('grid worker eval before init');
      }
      const t0 = performance.now();
      const ptr = ex.mandel_hvm2_grid(
        msg.centerRe,
        msg.centerIm,
        msg.scale,
        msg.w,
        msg.h,
        msg.blk,
        msg.bxN,
        msg.byN,
      );
      const ms = performance.now() - t0;
      const lastK =
        typeof ex.jshark_hvm2_last_k === 'function'
          ? ex.jshark_hvm2_last_k()
          : 0;
      const at = {
        centerRe: msg.centerRe,
        centerIm: msg.centerIm,
        scale: msg.scale,
        w: msg.w,
        h: msg.h,
      };
      if (!ptr) {
        g.postMessage({
          type: 'grid',
          ok: false,
          lastK,
          jobId,
          epoch,
          ms,
          ...at,
        });
        return;
      }
      const n = (msg.bxN | 0) * (msg.byN | 0);
      const copy = Int32Array.from(new Int32Array(memory.buffer, ptr, n));
      g.postMessage(
        { type: 'grid', ok: true, grid: copy, lastK, jobId, epoch, ms, ...at },
        [copy.buffer],
      );
    } catch (err) {
      fail(jobId, 'trap', err, epoch);
    }
  }
};
