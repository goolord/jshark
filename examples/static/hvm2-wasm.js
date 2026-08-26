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
}

function waitEvals(evalSync, count) {
  Atomics.store(evalSync, EVAL_DONE, 0);
  Atomics.store(evalSync, EVAL_ERR, 0);
  const deadline = performance.now() + 30000;
  while (Atomics.load(evalSync, EVAL_DONE) < count) {
    if (Atomics.load(evalSync, EVAL_ERR) !== 0) {
      throw new Error('HVM2 worker eval failed');
    }
    if (performance.now() > deadline) {
      throw new Error('HVM2 worker wait timeout');
    }
  }
  Atomics.store(evalSync, EVAL_DONE, 0);
  Atomics.store(evalSync, EVAL_ERR, 0);
}

async function fetchWasm(wasmUrl) {
  const r = await fetch(demoAssetUrl(wasmUrl));
  if (!r.ok) throw new Error('HVM2 wasm fetch failed: ' + wasmUrl);
  const wasmBytes = await r.arrayBuffer();
  const module = await WebAssembly.compile(wasmBytes);
  return { wasmBytes, module };
}

async function loadSingle(wasmUrl, loadNote = '') {
  const { wasmBytes, module } = await fetchWasm(wasmUrl);
  const memPages = importedMemoryPages(wasmBytes);
  const memory = new WebAssembly.Memory({
    initial: memPages.initial,
    maximum: memPages.maximum,
    shared: memPages.shared,
  });
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
    loadNote,
    blocking: false,
    terminate() {},
  };
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
  const memory = new WebAssembly.Memory({
    initial: memPages.initial,
    maximum: memPages.maximum,
    shared: memPages.shared,
  });

  const evalSync = new Int32Array(new SharedArrayBuffer(16));
  const workers = [];

  const jshark = {
    spawn_eval(tid, netPtr, bookPtr) {
      const w = workers[tid - 1];
      if (!w) return;
      w.postMessage({ type: 'eval', tid, netPtr, bookPtr });
    },
    wait_evals(count) {
      waitEvals(evalSync, count);
    },
    eval_done() {},
  };

  const instance = new WebAssembly.Instance(
    module,
    buildImports(module, jshark, memory),
  );

  const ex = instance.exports;
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
  }

  globalThis.__jsharkHvm2 = {
    exports: ex,
    memory,
    threads: tpc,
    loadMode: 'threaded',
    loadNote: '',
    blocking: tpc > 1,
    terminate() {
      for (const w of workers) w.terminate();
    },
  };
}

/**
 * Load HVM2 wasm. Uses threaded workers when COOP/COEP is active; otherwise
 * falls back to single-threaded SIMD (still supports wasm + hvm2 modes).
 */
globalThis.__jsharkHvm2Load = async (wasmUrl, workerUrl) => {
  if (globalThis.crossOriginIsolated && typeof SharedArrayBuffer !== 'undefined') {
    try {
      await loadThreaded(wasmUrl, workerUrl);
      return;
    } catch (err) {
      console.warn('HVM2 threaded load failed; falling back to single-thread', err);
      await loadSingle(wasmUrl, 'threads: 1 (threaded load failed)');
      return;
    }
  }
  await loadSingle(
    wasmUrl,
    'threads: 1 (COOP/COEP off)',
  );
};
