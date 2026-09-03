'use strict';

/** evalSync layout: [0]=done count, [1]=error flag (1=failed). */
const EVAL_DONE = 0;
const EVAL_ERR = 1;

/** Read imported memory min/max pages from a wasm binary (import section). */
function importedMemoryPages(wasmBytes) {
  const u8 =
    wasmBytes instanceof Uint8Array
      ? wasmBytes
      : new Uint8Array(wasmBytes);
  const view = new DataView(u8.buffer, u8.byteOffset, u8.byteLength);
  let pos = 8; // skip magic + version
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

function demoAssetUrl(rel) {
  const basePath = globalThis.location.pathname.replace(/\/?$/, '/');
  return new URL(rel, globalThis.location.origin + basePath).href;
}

/** @param {WebAssembly.Module} module */
function buildImports(module, jshark, memory) {
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
  const jsharkStubs = {
    spawn_eval: () => {},
    wait_evals: () => {},
    eval_done: () => {},
    reset_evals: () => {},
    live_threads: () => 1,
  };
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
        ns[imp.name] = jshark[imp.name] ?? jsharkStubs[imp.name] ?? (() => {});
      }
    } else if (imp.kind === 'memory') {
      if (!memory) {
        throw new Error('wasm import env.memory requires a Memory');
      }
      ns[imp.name] = memory;
    }
  }
  return out;
}

function checkExports(ex) {
  if (
    typeof ex.mandel_grid !== 'function' ||
    typeof ex.mandel_hvm2_grid !== 'function'
  ) {
    throw new Error('missing mandel_grid/mandel_hvm2_grid export');
  }
}

function signalEvalError(evalSync) {
  Atomics.store(evalSync, EVAL_ERR, 1);
  Atomics.add(evalSync, EVAL_DONE, 1);
  Atomics.notify(evalSync, EVAL_DONE);
}

function resetEvals(evalSync) {
  Atomics.store(evalSync, EVAL_DONE, 0);
  Atomics.store(evalSync, EVAL_ERR, 0);
}

function memNote(memPages) {
  return 'mem ' + memPages.initial + '..' + memPages.maximum + 'p';
}

function allocMemory(memPages) {
  try {
    return new WebAssembly.Memory({
      initial: memPages.initial,
      maximum: memPages.maximum,
      shared: memPages.shared,
    });
  } catch (err) {
    throw new Error(
      'HVM2 Memory alloc failed (' +
        memNote(memPages) +
        ', shared=' +
        !!memPages.shared +
        '): ' +
        (err && err.message ? err.message : String(err)),
    );
  }
}

/** Park on evalSync[DONE] instead of spinning the main thread. */
function waitEvals(evalSync, count, onTimeout) {
  const deadline = performance.now() + 30000;
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

async function fetchWasm(wasmUrl) {
  const r = await fetch(demoAssetUrl(wasmUrl));
  if (!r.ok) throw new Error('HVM2 wasm fetch failed: ' + wasmUrl);
  const wasmBytes = await r.arrayBuffer();
  const module = await WebAssembly.compile(wasmBytes);
  return { wasmBytes, module };
}

async function loadSingle(wasmUrl, workerUrl, loadNote = '') {
  const { wasmBytes, module } = await fetchWasm(wasmUrl);
  const memPages = importedMemoryPages(wasmBytes);
  const memory = allocMemory(memPages);
  /** @type {WebAssembly.Exports | null} */
  let ex = null;
  const jshark = {
    spawn_eval(tid, netPtr, bookPtr) {
      if (typeof ex?.jshark_worker_eval === 'function') {
        ex.jshark_worker_eval(tid, netPtr, bookPtr);
      }
    },
    wait_evals() {},
    eval_done() {},
    reset_evals() {},
    live_threads() {
      return 1;
    },
  };
  const instance = new WebAssembly.Instance(
    module,
    buildImports(module, jshark, memory),
  );
  ex = instance.exports;
  checkExports(ex);
  globalThis.__jsharkHvm2 = {
    exports: ex,
    memory,
    threads: 1,
    loadMode: 'single',
    loadNote: loadNote
      ? loadNote + ' · ' + memNote(memPages)
      : memNote(memPages),
    blocking: false,
    terminate() {},
  };
  startGridWorker(wasmBytes, workerUrl);
}

async function loadThreaded(wasmUrl, workerUrl) {
  if (!globalThis.crossOriginIsolated) {
    throw new Error('crossOriginIsolated required for threaded HVM2');
  }
  if (typeof SharedArrayBuffer === 'undefined') {
    throw new Error('SharedArrayBuffer unavailable');
  }

  const { wasmBytes, module } = await fetchWasm(wasmUrl);
  const memPages = importedMemoryPages(wasmBytes);
  const memory = allocMemory(memPages);

  const evalSync = new Int32Array(new SharedArrayBuffer(16));
  const workers = [];
  let live = 1;
  /** @type {WebAssembly.Exports | null} */
  let ex = null;

  const killPool = (reason) => {
    if (typeof ex?.jshark_cancel_eval === 'function') {
      try {
        ex.jshark_cancel_eval();
      } catch (_) {
        /* ignore */
      }
    }
    Atomics.store(evalSync, EVAL_ERR, 1);
    Atomics.notify(evalSync, EVAL_DONE);
    for (const w of workers) {
      try {
        w.terminate();
      } catch (_) {
        /* ignore */
      }
    }
    workers.length = 0;
    live = 1;
    const h = globalThis.__jsharkHvm2;
    if (h) {
      h.threads = 1;
      h.blocking = false;
      h.loadNote = 'threads: 1 (workers killed: ' + reason + ') · ' + memNote(memPages);
    }
  };

  const jshark = {
    spawn_eval(tid, netPtr, bookPtr) {
      const w = workers[tid - 1];
      if (!w) return;
      w.postMessage({ type: 'eval', tid, netPtr, bookPtr });
    },
    wait_evals(count) {
      waitEvals(evalSync, count, () => killPool('wait timeout'));
    },
    eval_done() {},
    reset_evals() {
      resetEvals(evalSync);
    },
    live_threads() {
      return live;
    },
  };

  const instance = new WebAssembly.Instance(
    module,
    buildImports(module, jshark, memory),
  );

  ex = instance.exports;
  checkExports(ex);
  if (!(memory.buffer instanceof SharedArrayBuffer)) {
    throw new Error('wasm memory is not shared (need COOP/COEP + threaded build)');
  }

  const tpc =
    typeof ex.jshark_tpc === 'function' ? ex.jshark_tpc() >>> 0 : 1;

  if (tpc > 1 && typeof ex.jshark_worker_eval === 'function') {
    const pool = tpc - 1;
    let ready = 0;
    const workerAbs = demoAssetUrl(workerUrl);
    await new Promise((resolve, reject) => {
      const timer = setTimeout(
        () => reject(new Error('HVM2 worker pool init timeout')),
        30000,
      );
      const fail = (err) => {
        clearTimeout(timer);
        reject(err instanceof Error ? err : new Error(String(err)));
      };
      for (let i = 0; i < pool; i++) {
        const w = new Worker(workerAbs);
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
            signalEvalError(evalSync);
            fail(new Error('HVM2 worker ' + m.workerId + ': ' + m.message));
            return;
          }
          if (m.type === 'ready') {
            ready++;
            if (ready === pool) {
              clearTimeout(timer);
              resolve();
            }
          }
        };
        w.onerror = (ev) => {
          signalEvalError(evalSync);
          const where =
            ev.filename && ev.lineno
              ? ev.filename + ':' + ev.lineno + (ev.colno ? ':' + ev.colno : '')
              : workerAbs;
          fail(
            new Error(
              'HVM2 worker failed (' +
                where +
                '): ' +
                (ev.message || 'script load or parse error'),
            ),
          );
        };
        w.onmessageerror = () => {
          if (initAttempt === 0) {
            initAttempt = 1;
            sendInit(false);
          } else {
            fail(new Error('HVM2 worker init transfer failed'));
          }
        };
        workers[i] = w;
        sendInit(true);
      }
    });
    live = tpc;
  }

  globalThis.__jsharkHvm2 = {
    exports: ex,
    memory,
    threads: live,
    loadMode: 'threaded',
    loadNote:
      (live > 1 ? 'threads: ' + live : 'threads: 1') + ' · ' + memNote(memPages),
    blocking: live > 1,
    terminate() {
      for (const w of workers) w.terminate();
    },
  };
  startGridWorker(wasmBytes, workerUrl);
}

/**
 * Load HVM2 wasm. Shared-memory workers when COOP/COEP is on (TPC from
 * jshark_tpc); otherwise one thread (wasm + hvm2 modes still work).
 */
globalThis.__jsharkHvm2Load = async (wasmUrl, workerUrl) => {
  if (globalThis.crossOriginIsolated && typeof SharedArrayBuffer !== 'undefined') {
    try {
      await loadThreaded(wasmUrl, workerUrl);
      return;
    } catch (err) {
      console.warn('HVM2 threaded load failed; falling back to single-thread', err);
      await loadSingle(
        wasmUrl,
        workerUrl,
        'threads: 1 (threaded load failed: ' +
          (err && err.message ? err.message : String(err)) +
          ')',
      );
      return;
    }
  }
  await loadSingle(
    wasmUrl,
    workerUrl,
    'threads: 1 (COOP/COEP off)',
  );
};

function gridWorkerUrl(workerUrl) {
  return String(workerUrl || '').replace(
    /hvm2-worker\.js/,
    'hvm2-grid-worker.js',
  );
}

function stopGridWorker(w) {
  if (!w) {
    return;
  }
  try {
    w.postMessage({ type: 'die' });
  } catch (_) {
    try {
      w.terminate();
    } catch (_) {
      /* ignore */
    }
  }
}

function startGridWorker(wasmBytes, workerUrl) {
  const rel = gridWorkerUrl(workerUrl);
  if (!rel || rel === String(workerUrl || '')) {
    return;
  }
  stopGridWorker(globalThis.__jsharkHvm2GridWorker);
  globalThis.__jsharkHvm2GridReady = false;
  globalThis.__jsharkHvm2Job = 0;
  globalThis.__jsharkHvm2GridError = null;
  globalThis.__jsharkHvm2CacheAt = null;
  globalThis.__jsharkHvm2CacheGen = 0;
  globalThis.__jsharkHvm2GridThreads = 0;
  globalThis.__jsharkHvm2LastReq = null;
  globalThis.__jsharkHvm2Epoch = globalThis.__jsharkHvm2Epoch | 0;
  let jobSeq = 0;
  let benchWait = null;
  let w;
  try {
    w = new Worker(demoAssetUrl(rel));
  } catch (err) {
    console.warn('HVM2 grid worker spawn failed', err);
    return;
  }
  const finishJob = (m) => {
    if (m.jobId === globalThis.__jsharkHvm2Job) {
      globalThis.__jsharkHvm2Job = 0;
    }
    if (benchWait && benchWait.jobId === m.jobId) {
      const resolve = benchWait.resolve;
      benchWait = null;
      resolve(m.ok && m.ms >= 0 ? Math.round(m.ms * 10) / 10 : -1);
    }
  };
  w.onmessage = (ev) => {
    const m = ev.data;
    if (m.type === 'dead') {
      try {
        w.terminate();
      } catch (_) {
        /* ignore */
      }
      return;
    }
    if (m.type === 'ready') {
      globalThis.__jsharkHvm2GridReady = true;
      globalThis.__jsharkHvm2GridThreads = m.threads || 1;
      return;
    }
    if (m.type === 'error') {
      if (m.jobId && m.jobId !== globalThis.__jsharkHvm2Job) {
        return;
      }
      finishJob(m);
      if ((m.epoch | 0) !== (globalThis.__jsharkHvm2Epoch | 0)) {
        return;
      }
      globalThis.__jsharkHvm2GridError = m.kind || 'trap';
      globalThis.__jsharkHvm2LastK = -15;
      console.warn('HVM2 grid worker', m.message);
      return;
    }
    if (m.type === 'grid') {
      finishJob(m);
      if ((m.epoch | 0) !== (globalThis.__jsharkHvm2Epoch | 0)) {
        return;
      }
      globalThis.__jsharkHvm2LastK = m.lastK;
      globalThis.__jsharkHvm2LastMs = m.ms;
      if (m.ok && m.grid) {
        globalThis.__jsharkHvm2Cache = m.grid;
        globalThis.__jsharkHvm2CacheAt = {
          centerRe: m.centerRe,
          centerIm: m.centerIm,
          scale: m.scale,
          w: m.w,
          h: m.h,
        };
        globalThis.__jsharkHvm2CacheGen =
          (globalThis.__jsharkHvm2CacheGen | 0) + 1;
        globalThis.__jsharkHvm2GridError = null;
      } else {
        globalThis.__jsharkHvm2GridError = 'zero';
      }
    }
  };
  w.onerror = (ev) => {
    globalThis.__jsharkHvm2GridReady = false;
    globalThis.__jsharkHvm2Job = 0;
    globalThis.__jsharkHvm2GridError = 'trap';
    if (benchWait) {
      const resolve = benchWait.resolve;
      benchWait = null;
      resolve(-1);
    }
    console.warn('HVM2 grid worker failed', ev.message);
  };
  w.postMessage({
    type: 'init',
    wasmBytes,
    workerUrl: demoAssetUrl(workerUrl),
  });
  globalThis.__jsharkHvm2GridWorker = w;
  globalThis.__jsharkHvm2RequestGrid = (args) => {
    if (!globalThis.__jsharkHvm2GridReady || globalThis.__jsharkHvm2Job) {
      return;
    }
    const key = [
      args.centerRe,
      args.centerIm,
      args.scale,
      args.w,
      args.h,
      args.bxN,
      args.byN,
    ].join(',');
    if (key === globalThis.__jsharkHvm2LastReq) {
      return;
    }
    globalThis.__jsharkHvm2LastReq = key;
    const jobId = ++jobSeq;
    const epoch = globalThis.__jsharkHvm2Epoch | 0;
    globalThis.__jsharkHvm2Job = jobId;
    w.postMessage({ type: 'grid', jobId, epoch, ...args });
  };
  globalThis.__jsharkHvm2BenchGrid = (args) =>
    new Promise((resolve) => {
      if (!globalThis.__jsharkHvm2GridReady) {
        resolve(-1);
        return;
      }
      if (globalThis.__jsharkHvm2Job) {
        resolve(globalThis.__jsharkHvm2LastMs >= 0
          ? globalThis.__jsharkHvm2LastMs
          : -1);
        return;
      }
      const jobId = ++jobSeq;
      const epoch = globalThis.__jsharkHvm2Epoch | 0;
      globalThis.__jsharkHvm2Job = jobId;
      globalThis.__jsharkHvm2LastReq = null;
      benchWait = { jobId, resolve };
      w.postMessage({ type: 'grid', jobId, epoch, ...args });
    });
  const prevTerm = globalThis.__jsharkHvm2?.terminate;
  if (globalThis.__jsharkHvm2) {
    globalThis.__jsharkHvm2.terminate = () => {
      if (typeof prevTerm === 'function') {
        prevTerm();
      }
      stopGridWorker(w);
    };
  }
}
