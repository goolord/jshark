{-# LANGUAGE OverloadedStrings #-}

-- | HVM2 demo WASM pipeline: demo-specific Bend module assembly and the
-- C/WASM bridge appended to @bend gen-c@ output.
--
-- The generic Bend emitter lives in @JShark.Compiler.EmitBend@; everything
-- here is Mandelbrot-demo payload: the @jshark_grid@ driver, the
-- 4096-leaf parallel @main@, the SIMD128 fast path, and the HVM2
-- net-reduction grid export.
module JShark.Example.Hvm2Demo.WasmBuild
  ( Hvm2Config (..)
  , defaultHvm2Config
  , defaultWasmBuildDir
  , demoBendModule
  , compileHvm2GenC
  , compileHvm2Wasm
  , emitKernelWasmBridge
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import JShark (irExprFromClosed)
import JShark.Api.Types (Hvm2KernelEntry (..))
import JShark.Compiler.EmitBend
  ( Hvm2Error (..)
  , bendDefExports
  , emitBendKernel
  , emitKernelExportsC
  , sanitizeBendId
  , sanitizeKernelCForWasm
  )
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)

-- | Tool paths for the HVM2 build pipeline.
data Hvm2Config = Hvm2Config
  { hvm2BendExe :: !FilePath
  , hvm2ZigExe :: !FilePath
  , hvm2WasmBuildDir :: !FilePath
  }
  deriving (Eq, Show)

defaultWasmBuildDir :: FilePath
defaultWasmBuildDir = "wasm/hvm2"

defaultHvm2Config :: Hvm2Config
defaultHvm2Config =
  Hvm2Config
    { hvm2BendExe = "bend"
    , hvm2ZigExe = "zig"
    , hvm2WasmBuildDir = defaultWasmBuildDir
    }

demoBendModule :: [Hvm2KernelEntry] -> Either Hvm2Error Text
demoBendModule entries = do
  defs <- traverse emitEntry entries
  emitBendModuleFromDefs defs
 where
  emitEntry (Hvm2KernelEntry name k) =
    emitBendKernel name (irExprFromClosed k)

emitBendModuleFromDefs :: [Text] -> Either Hvm2Error Text
emitBendModuleFromDefs defs =
  pure $
    T.unlines (prelude <> defs <> gridLines <> [""] <> mainLines)
      <> "\n"
 where
  exports = bendDefExports (T.unlines defs)
  prelude = case exports of
    [] -> []
    _ ->
      [ "# Balanced result tree; ~fields are recursive, so `fold` reduces"
      , "# both children of every Node independently (in parallel)."
      , "type ParTree:"
      , "  Node { ~lhs: ParTree, ~rhs: ParTree }"
      , "  Leaf { val: u24 }"
      , ""
      ]
  -- Whole-frame HVM2 driver: one net computes every pixel of a bxN x byN
  -- grid as a balanced tuple tree. The WASM bridge normalizes it once per
  -- frame and walks the tree into linear memory, so the browser demo runs
  -- the Bend-compiled kernel itself, not a C reimplementation.
  gridLines = case exports of
    (name, 2) : _ ->
      [ ""
      , "# Untyped on purpose: interior nodes are (lhs, rhs) tuples, leaves"
      , "# are f24 kernel results; the C bridge walks CON/NUM ports directly."
      , "def jshark_grid(cRe, cIm, scale, w, h, blk, bxN, byN):"
      , "  nx = f24/to_u24(bxN)"
      , "  bend lo = 0, hi = f24/to_u24(bxN * byN):"
      , "    when (hi - lo) > 1:"
      , "      acc = (fork(lo, (lo + hi) / 2), fork((lo + hi) / 2, hi))"
      , "    else:"
      , "      acc = "
          <> name
          <> "(cRe + (((u24/to_f24(lo % nx) * blk) + (blk / 2.0)) - (w / 2.0)) * scale / w, "
          <> "cIm + (((u24/to_f24(lo / nx) * blk) + (blk / 2.0)) - (h / 2.0)) * scale / h)"
      , "  return acc"
      ]
    _ -> []
  mainLines = case exports of
    [] -> ["def main():", "  return 0"]
    (name, arity) : _ ->
      let
        leafArgs
          | arity == 2 =
              "(u24/to_f24(lo % 64) / 32.0) - 2.0, (u24/to_f24(lo / 64) / 32.0) - 1.0"
          | otherwise = T.intercalate ", " (replicate arity "u24/to_f24(lo)")
        leafCall = name <> "(" <> leafArgs <> ")"
       in
        [ "# Parallel driver: `bend` builds a 4096-leaf tree of independent"
        , "# kernel calls (a 64×64 grid over the classic view); the `fold` in"
        , "# sum_tree is a parallel reduction over that tree."
        , "def main():"
        , "  def sum_tree(t: ParTree) -> u24:"
        , "    fold t:"
        , "      case ParTree/Node:"
        , "        return t.lhs + t.rhs"
        , "      case ParTree/Leaf:"
        , "        return t.val"
        , "  bend lo = 0, hi = 4096:"
        , "    when (hi - lo) > 1:"
        , "      acc = ParTree/Node(fork(lo, (lo + hi) / 2), fork((lo + hi) / 2, hi))"
        , "    else:"
        , "      acc = ParTree/Leaf(f24/to_u24(" <> leafCall <> "))"
        , "  return sum_tree(acc)"
        ]

-- | Bend → HVM2 C. Writes @kernel.bend@, @kernel.c@, and @kernel_exports.c@
-- under @outDir@.
compileHvm2GenC ::
  Hvm2Config
  -> FilePath
  -> Int
  -> Text
  -> IO (Either Hvm2Error FilePath)
compileHvm2GenC cfg outDir maxIter bendSrc = do
  createDirectoryIfMissing True outDir
  let
    bendPath = outDir </> "kernel.bend"
    cPath = outDir </> "kernel.c"
    exportsPath = outDir </> "kernel_exports.c"
  T.writeFile bendPath bendSrc
  T.writeFile exportsPath (emitKernelExportsC (bendDefExports bendSrc))
  -- All bend optimization passes that preserve callable-by-name defs.
  -- @prune@ is deliberately absent: it deletes defs unreachable from main,
  -- and the WASM bridge invokes @jshark_grid@ by name at runtime.
  let
    bendOpts =
      concatMap
        (\o -> ["-O", o])
        ["eta", "merge", "inline", "linearize-matches", "float-combinators"]
  genRes <-
    readProcessWithExitCode
      (hvm2BendExe cfg)
      (["gen-c"] <> bendOpts <> [bendPath])
      ""
  case genRes of
    (ExitSuccess, out, _) -> do
      let
        exports = bendDefExports bendSrc
        kernelC =
          sanitizeKernelCForWasm (T.pack out)
            <> "\n"
            <> emitKernelWasmBridge maxIter exports
      T.writeFile cPath kernelC
      pure (Right cPath)
    (ExitFailure code, out, err) ->
      pure $
        Left
          ( Hvm2Unsupported
              ( "bend gen-c failed (exit "
                  <> T.pack (show code)
                  <> "): "
                  <> T.pack (out ++ err)
              )
          )

-- | Full pipeline: Bend → C → WASM via Zig.
compileHvm2Wasm ::
  Hvm2Config
  -> FilePath
  -> Int
  -> Text
  -> IO (Either Hvm2Error FilePath)
compileHvm2Wasm cfg outDir maxIter bendSrc = do
  cRes <- compileHvm2GenC cfg outDir maxIter bendSrc
  case cRes of
    Left err -> pure (Left err)
    Right cPath -> do
      let
        wasmPath = outDir </> "bin" </> "jshark-hvm2.wasm"
        buildDir = hvm2WasmBuildDir cfg
        buildFile = buildDir </> "build.zig"
        exportsPath = outDir </> "kernel_exports.c"
      createDirectoryIfMissing True (takeDirectory wasmPath)
      absCPath <- makeAbsolute cPath
      absExportsPath <- makeAbsolute exportsPath
      absBuildFile <- makeAbsolute buildFile
      zigRes <-
        readProcessWithExitCode
          (hvm2ZigExe cfg)
          [ "build"
          , "--build-file"
          , absBuildFile
          , "-Doptimize=ReleaseFast"
          , "-Dtpc-l2=2"
          , "-Dkernel-c=" ++ absCPath
          , "-Dexports-c=" ++ absExportsPath
          , "--prefix"
          , outDir
          ]
          ""
      case zigRes of
        (ExitSuccess, _, _) -> pure (Right wasmPath)
        (ExitFailure code, out, err) ->
          pure $
            Left
              ( Hvm2Unsupported
                  ( "zig build failed (exit "
                      <> T.pack (show code)
                      <> "): "
                      <> T.pack (out ++ err)
                  )
              )

-- | HVM2 export shims appended to @kernel.c@ (same translation unit).
-- Two families per kernel: fast C/SIMD exports (@name@, @name_f64@,
-- @name_grid@) matching 'JShark.Example.Hvm2Demo.Kernels.mandelJsSource',
-- and @name_hvm2_grid@, which reduces the Bend-compiled book itself via the
-- HVM2 evaluator.
emitKernelWasmBridge :: Int -> [(Text, Int)] -> Text
emitKernelWasmBridge maxIter exports =
  let
    bad = filter ((/= 2) . snd) exports
   in
    if not (null bad)
      then
        error $
          "emitKernelWasmBridge: kernel arity must be 2, got: "
            ++ show bad
      else
        T.unlines $
          [ "/* --- JShark HVM2 WASM bridge (auto-generated) --- */"
          , "typedef int64_t jshark_hvm2_i64;"
          , ""
          , "#define JSHARK_MANDEL_MAX_ITER " <> T.pack (show maxIter)
          , ""
          , "static jshark_hvm2_i64 jshark_mandel_iter(double cr, double ci) {"
          , "  int n = 0;"
          , "  double zr = 0.0;"
          , "  double zi = 0.0;"
          , "  while (n < JSHARK_MANDEL_MAX_ITER && (zr * zr + zi * zi) < 4.0) {"
          , "    double nzr = zr * zr - zi * zi + cr;"
          , "    double nzi = 2.0 * zr * zi + ci;"
          , "    zr = nzr;"
          , "    zi = nzi;"
          , "    n++;"
          , "  }"
          , "  return (jshark_hvm2_i64)n;"
          , "}"
          , ""
          , "static double jshark_i64_to_f64(jshark_hvm2_i64 x) {"
          , "  union { jshark_hvm2_i64 i; double d; } u;"
          , "  u.i = x;"
          , "  return u.d;"
          , "}"
          , ""
          , "/* Batched grid: one JS->WASM call per frame instead of one per pixel,"
          , " * so kernel timing reflects compute, not boundary-crossing overhead. */"
          , "#define JSHARK_GRID_CAP (1 << 17)"
          , "static int32_t jshark_grid_buf[JSHARK_GRID_CAP];"
          , ""
          , "/* SIMD128 quad kernel: four pixels via two interleaved f64x2 vectors."
          , " * A warmed-up JS JIT matches scalar WASM on this loop; vectorization"
          , " * plus the ILP from two independent dependency chains is the edge JS"
          , " * cannot replicate. Arithmetic order mirrors jshark_mandel_iter so"
          , " * results stay bit-identical to the scalar path. */"
          , "#ifdef __wasm_simd128__"
          , "#include <wasm_simd128.h>"
          , "static void jshark_mandel_quad(double cr0, double cr1, double cr2,"
          , "    double cr3, double ci, int32_t *out) {"
          , "  v128_t crA = wasm_f64x2_make(cr0, cr1);"
          , "  v128_t crB = wasm_f64x2_make(cr2, cr3);"
          , "  v128_t civ = wasm_f64x2_splat(ci);"
          , "  v128_t zrA = wasm_f64x2_splat(0.0);"
          , "  v128_t ziA = zrA;"
          , "  v128_t zrB = zrA;"
          , "  v128_t ziB = zrA;"
          , "  v128_t four = wasm_f64x2_splat(4.0);"
          , "  v128_t two = wasm_f64x2_splat(2.0);"
          , "  v128_t itA = wasm_i64x2_splat(0);"
          , "  v128_t itB = itA;"
          , "  for (int k = 0; k < JSHARK_MANDEL_MAX_ITER; k++) {"
          , "    v128_t zr2A = wasm_f64x2_mul(zrA, zrA);"
          , "    v128_t zi2A = wasm_f64x2_mul(ziA, ziA);"
          , "    v128_t zr2B = wasm_f64x2_mul(zrB, zrB);"
          , "    v128_t zi2B = wasm_f64x2_mul(ziB, ziB);"
          , "    v128_t actA = wasm_f64x2_lt(wasm_f64x2_add(zr2A, zi2A), four);"
          , "    v128_t actB = wasm_f64x2_lt(wasm_f64x2_add(zr2B, zi2B), four);"
          , "    if (!wasm_v128_any_true(wasm_v128_or(actA, actB))) break;"
          , "    /* active lanes are all-ones (-1); subtracting increments them */"
          , "    itA = wasm_i64x2_sub(itA, actA);"
          , "    itB = wasm_i64x2_sub(itB, actB);"
          , "    v128_t nzrA = wasm_f64x2_add(wasm_f64x2_sub(zr2A, zi2A), crA);"
          , "    v128_t nziA = wasm_f64x2_add("
          , "        wasm_f64x2_mul(wasm_f64x2_mul(two, zrA), ziA), civ);"
          , "    v128_t nzrB = wasm_f64x2_add(wasm_f64x2_sub(zr2B, zi2B), crB);"
          , "    v128_t nziB = wasm_f64x2_add("
          , "        wasm_f64x2_mul(wasm_f64x2_mul(two, zrB), ziB), civ);"
          , "    zrA = wasm_v128_bitselect(nzrA, zrA, actA);"
          , "    ziA = wasm_v128_bitselect(nziA, ziA, actA);"
          , "    zrB = wasm_v128_bitselect(nzrB, zrB, actB);"
          , "    ziB = wasm_v128_bitselect(nziB, ziB, actB);"
          , "  }"
          , "  out[0] = (int32_t)wasm_i64x2_extract_lane(itA, 0);"
          , "  out[1] = (int32_t)wasm_i64x2_extract_lane(itA, 1);"
          , "  out[2] = (int32_t)wasm_i64x2_extract_lane(itB, 0);"
          , "  out[3] = (int32_t)wasm_i64x2_extract_lane(itB, 1);"
          , "}"
          , "#endif"
          , ""
          , "/* --- true HVM2 execution ---"
          , " * Runs the Bend-compiled book itself (interaction-net reduction), not"
          , " * the C fast path above. One long-lived net is booted lazily and reset"
          , " * between calls by clearing only the high-water region each run used."
          , " * jshark_parallel_normalize() drives all TPC slots (browser wasm uses"
          , " * shared memory + Web Workers when COOP/COEP is enabled). */"
          , "static Book* jshark_hvm2_book = NULL;"
          , "static Net* jshark_hvm2_net = NULL;"
          , "static int jshark_hvm2_last_k = 0;"
          , "static Book jshark_hvm2_book_storage;"
          , "static Net jshark_hvm2_net_storage;"
          , ""
          , "static int jshark_hvm2_boot(void) {"
          , "  if (jshark_hvm2_book) { return jshark_hvm2_net != NULL; }"
          , "  alloc_static_tms();"
          , "  jshark_hvm2_book = &jshark_hvm2_book_storage;"
          , "  memset(jshark_hvm2_book, 0, sizeof(Book));"
          , "  if (!book_load(jshark_hvm2_book, (u32*)BOOK_BUF)) {"
          , "    jshark_hvm2_last_k = -11;"
          , "    jshark_hvm2_book = NULL;"
          , "    return 0;"
          , "  }"
          , "  jshark_hvm2_net = &jshark_hvm2_net_storage;"
          , "  memset(jshark_hvm2_net, 0, sizeof(Net));"
          , "  return 1;"
          , "}"
          , ""
          , "static u32 jshark_hvm2_def_id(const char* name) {"
          , "  Book* book = jshark_hvm2_book;"
          , "  if (!book) { return 0xFFFFFFFF; }"
          , "  for (u32 i = 0; i < 32; ++i) {"
          , "    const char* dn = book->defs_buf[i].name;"
          , "    if (dn[0] == 0) { continue; }"
          , "    u32 j = 0;"
          , "    while (name[j] != 0 && dn[j] != 0 && name[j] == dn[j]) { j++; }"
          , "    if (name[j] == 0 && dn[j] == 0) { return i; }"
          , "  }"
          , "  return 0xFFFFFFFF;"
          , "}"
          , ""
          , "static void jshark_hvm2_reset(void) {"
          , "  Net* net = jshark_hvm2_net;"
          , "  /* HVM alloc is tid*(G_NODE_LEN/TPC) + nput%part — clear each slice. */"
          , "  u32 part = G_NODE_LEN / TPC;"
          , "  if (part == 0) { part = G_NODE_LEN; }"
          , "  for (u32 ti = 0; ti < TPC; ++ti) {"
          , "    u32 n = tm[ti]->nput + 16;"
          , "    u32 v = tm[ti]->vput + 16;"
          , "    if (n > part) { n = part; }"
          , "    if (v > part) { v = part; }"
          , "    memset((void*)(net->node_buf + (u64)ti * part), 0,"
          , "        sizeof(net->node_buf[0]) * (u64)n);"
          , "    memset((void*)(net->vars_buf + (u64)ti * part), 0,"
          , "        sizeof(net->vars_buf[0]) * (u64)v);"
          , "  }"
          , "  memset((void*)net->rbag_buf, 0, sizeof(net->rbag_buf));"
          , "  vars_create(net, get_val(ROOT), 0);"
          , "  atomic_store(&net->itrs, 0);"
          , "  atomic_store(&net->idle, 0);"
          , "  for (u32 ti = 0; ti < TPC; ++ti) {"
          , "    TM* t = tm[ti];"
          , "    t->itrs = 0;"
          , "    t->nput = 1;"
          , "    t->vput = 1;"
          , "    t->rput = 0;"
          , "    t->hput = 0;"
          , "    t->sidx = 0;"
          , "  }"
          , "}"
          , ""
          , "/* Applies an 8-ary def to f24 args and normalizes:"
          , " *   @def ~ (a0 (a1 ... (a7 ROOT)))"
          , " * then walks the resulting balanced tuple tree (CON = branch, NUM ="
          , " * leaf) in order into jshark_grid_buf. Returns leaves written, or -1. */"
          , "__attribute__((import_module(\"jshark\"), import_name(\"spawn_eval\")))"
          , "void jshark_import_spawn_eval(u32 tid, u32 net_ptr, u32 book_ptr);"
          , ""
          , "__attribute__((import_module(\"jshark\"), import_name(\"wait_evals\")))"
          , "void jshark_import_wait_evals(u32 count);"
          , ""
          , "__attribute__((import_module(\"jshark\"), import_name(\"eval_done\")))"
          , "void jshark_import_eval_done(void);"
          , ""
          , "__attribute__((import_module(\"jshark\"), import_name(\"reset_evals\")))"
          , "void jshark_import_reset_evals(void);"
          , ""
          , "__attribute__((import_module(\"jshark\"), import_name(\"live_threads\")))"
          , "u32 jshark_import_live_threads(void);"
          , ""
          , "static void jshark_parallel_normalize(Net* net, Book* book, u32 budget);"
          , ""
          , "/* Do not call HVM evaluator(): it resets idle and sync_threads()"
          , " * for all TPC slots, so a late worker deadlocks the barrier. */"
          , "static volatile u32 jshark_eval_cancel = 0;"
          , "static u32 jshark_shared_budget = 0;"
          , ""
          , "static int jshark_bags_empty(Net* net) {"
          , "  u32 ti;"
          , "  for (ti = 0; ti < TPC; ++ti) {"
          , "    if (rbag_len(net, tm[ti]) > 0) { return 0; }"
          , "  }"
          , "  return 1;"
          , "}"
          , ""
          , "static void jshark_steal_eval(Net* net, TM* t, Book* book, u32 budget) {"
          , "  u32 miss = 0;"
          , "  u32 part = G_RBAG_LEN / TPC;"
          , "  if (part == 0) { part = 1; }"
          , "  while (budget-- > 0) {"
          , "    Pair got;"
          , "    u32 sid;"
          , "    u32 idx;"
          , "    if (jshark_eval_cancel) { return; }"
          , "    if (rbag_len(net, t) > 0) {"
          , "      miss = 0;"
          , "      if (!interact(net, t, book)) {"
          , "        jshark_hvm2_last_k = -13;"
          , "        return;"
          , "      }"
          , "      continue;"
          , "    }"
          , "    sid = (t->tid + 1) % TPC;"
          , "    idx = sid * part + (t->sidx % part);"
          , "    t->sidx++;"
          , "    got = atomic_exchange_explicit("
          , "        &net->rbag_buf[idx], 0, memory_order_relaxed);"
          , "    if (got != 0) {"
          , "      push_redex(net, t, got);"
          , "      miss = 0;"
          , "      continue;"
          , "    }"
          , "    miss++;"
          , "    /* G_RBAG_LEN is TPC*RLEN; waiting for part*TPC misses"
          , "     * exceeds the small-grid budget and yields a black frame. */"
          , "    if ((miss & 127u) == 0 && jshark_bags_empty(net)) { return; }"
          , "    if (miss >= part * TPC) { miss = 0; }"
          , "  }"
          , "  jshark_hvm2_last_k = -14;"
          , "}"
          , ""
          , "static void jshark_wasm_normalize(Net* net, Book* book, u32 budget) {"
          , "  jshark_steal_eval(net, tm[0], book, budget);"
          , "}"
          , ""
          , "static u32 jshark_norm_budget(int cells) {"
          , "  u32 cap = (u32)(cells > 0 ? cells : 0);"
          , "  u32 b = cap * 8192u + 65536u;"
          , "  if (b < 200000u) { b = 200000u; }"
          , "  if (b > 50000000u) { b = 50000000u; }"
          , "  return b;"
          , "}"
          , ""
          , "/* Interactive cap. 4096 leaves lock the tab on one normalize;"
          , " * 256 still shows the net and upsamples to the JS blit size. */"
          , "#define JSHARK_HVM2_MAX_CELLS 256"
          , "static int32_t jshark_hvm2_scratch[JSHARK_HVM2_MAX_CELLS];"
          , ""
          , "static void jshark_hvm2_fit_grid(int nx, int ny, int* fnx, int* fny) {"
          , "  int x = nx;"
          , "  int y = ny;"
          , "  while ((long)x * (long)y > JSHARK_HVM2_MAX_CELLS && (x > 1 || y > 1)) {"
          , "    if (x > 1) { x = (x * 7) / 8; if (x < 1) { x = 1; } }"
          , "    if (y > 1) { y = (y * 7) / 8; if (y < 1) { y = 1; } }"
          , "  }"
          , "  while ((long)(x + 1) * (long)y <= JSHARK_HVM2_MAX_CELLS && x < nx) {"
          , "    x++;"
          , "  }"
          , "  while ((long)x * (long)(y + 1) <= JSHARK_HVM2_MAX_CELLS && y < ny) {"
          , "    y++;"
          , "  }"
          , "  *fnx = x;"
          , "  *fny = y;"
          , "}"
          , ""
          , "static void jshark_hvm2_upsample(int fnx, int fny, int nx, int ny) {"
          , "  int n;"
          , "  int i;"
          , "  int by;"
          , "  if (fnx == nx && fny == ny) { return; }"
          , "  n = fnx * fny;"
          , "  for (i = 0; i < n; i++) {"
          , "    jshark_hvm2_scratch[i] = jshark_grid_buf[i];"
          , "  }"
          , "  for (by = 0; by < ny; by++) {"
          , "    int sy = by * fny / ny;"
          , "    int bx;"
          , "    for (bx = 0; bx < nx; bx++) {"
          , "      int sx = bx * fnx / nx;"
          , "      jshark_grid_buf[by * nx + bx] = jshark_hvm2_scratch[sy * fnx + sx];"
          , "    }"
          , "  }"
          , "}"
          , ""
          , "static int jshark_hvm2_run_grid(u32 fid, const double* args, int cap) {"
          , "  Net* net = jshark_hvm2_net;"
          , "  jshark_hvm2_last_k = 0;"
          , "  jshark_hvm2_reset();"
          , "  vars_create(net, get_val(ROOT), NONE);"
          , "  for (int i = 0; i < 8; ++i) {"
          , "    Port cont = (i == 7) ? ROOT : new_port(CON, (u32)(i + 2));"
          , "    Port argp = new_port(NUM, new_f24((float)args[i]));"
          , "    node_create(net, (u32)(i + 1), new_pair(argp, cont));"
          , "  }"
          , "  net->rbag_buf[0] = 0;"
          , "  push_redex(net, tm[0], new_pair(new_port(REF, fid), new_port(CON, 1)));"
          , "  jshark_parallel_normalize(net, jshark_hvm2_book, jshark_norm_budget(cap));"
          , "  if (jshark_hvm2_last_k < 0) { return jshark_hvm2_last_k; }"
          , "  Port stack[64];"
          , "  int sp = 0;"
          , "  int k = 0;"
          , "  int walk_lim = cap * 32 + 128;"
          , "  stack[sp++] = ROOT;"
          , "  while (sp > 0) {"
          , "    if (--walk_lim <= 0) { return -1; }"
          , "    Port p = enter(net, stack[--sp]);"
          , "    if (get_tag(p) == CON) {"
          , "      if (sp + 2 > 64) { return -1; }"
          , "      Pair nd = node_load(net, get_val(p));"
          , "      stack[sp++] = get_snd(nd);"
          , "      stack[sp++] = get_fst(nd);"
          , "      continue;"
          , "    }"
          , "    if (get_tag(p) == NUM) {"
          , "      Numb nb = get_val(p);"
          , "      u32 ty = get_typ(nb);"
          , "      int32_t v = (ty == TY_F24) ? (int32_t)get_f24(nb)"
          , "                                 : (int32_t)get_u24(nb);"
          , "      if (k >= cap) { return -1; }"
          , "      jshark_grid_buf[k++] = v;"
          , "      continue;"
          , "    }"
          , "    return -1;"
          , "  }"
          , "  return k;"
          , "}"
          , ""
          , "static void jshark_parallel_normalize(Net* net, Book* book, u32 budget) {"
          , "  /* live_threads is host hardwareConcurrency capped at TPC. */"
          , "  u32 live = jshark_import_live_threads();"
          , "  u32 cap;"
          , "  u32 spawned = 0;"
          , "  u32 t;"
          , "  jshark_eval_cancel = 0;"
          , "  jshark_shared_budget = budget;"
          , "  if (live <= 1) {"
          , "    jshark_wasm_normalize(net, book, budget);"
          , "    return;"
          , "  }"
          , "  cap = live < (u32)TPC ? live : (u32)TPC;"
          , "  jshark_import_reset_evals();"
          , "  for (t = 1; t < cap; ++t) {"
          , "    jshark_import_spawn_eval(t, (u32)(uintptr_t)net, (u32)(uintptr_t)book);"
          , "    spawned++;"
          , "  }"
          , "  jshark_steal_eval(net, tm[0], book, budget);"
          , "  jshark_import_wait_evals(spawned);"
          , "}"
          , ""
          , "__attribute__((export_name(\"jshark_worker_eval\")))"
          , "void jshark_worker_eval(u32 tid, u32 net_ptr, u32 book_ptr) {"
          , "  jshark_steal_eval((Net*)(uintptr_t)net_ptr, tm[tid],"
          , "      (Book*)(uintptr_t)book_ptr, jshark_shared_budget);"
          , "  jshark_import_eval_done();"
          , "}"
          , ""
          , "__attribute__((export_name(\"jshark_cancel_eval\")))"
          , "void jshark_export_cancel_eval(void) { jshark_eval_cancel = 1; }"
          , ""
          , "__attribute__((export_name(\"jshark_tpc\")))"
          , "u32 jshark_export_tpc(void) { return (u32)TPC; }"
          , ""
          ]
            <> concatMap exportBridge exports
            <> [""]
 where
  exportBridge (name, arity)
    | arity /= 2 = []
    | otherwise =
        let
          bendId = sanitizeBendId name
          cSym = cIdent name
         in
          [ "__attribute__((export_name(\"" <> bendId <> "\")))"
          , "jshark_hvm2_i64 jshark_export_"
              <> cSym
              <> "(jshark_hvm2_i64 a0, jshark_hvm2_i64 a1) {"
          , "  return jshark_mandel_iter(jshark_i64_to_f64(a0), jshark_i64_to_f64(a1));"
          , "}"
          , ""
          , "__attribute__((export_name(\""
              <> bendId
              <> "_f64\")))"
          , "double jshark_export_"
              <> cSym
              <> "_f64(double a0, double a1) {"
          , "  return (double)jshark_mandel_iter(a0, a1);"
          , "}"
          , ""
          , "__attribute__((export_name(\""
              <> bendId
              <> "_grid\")))"
          , "int32_t jshark_export_"
              <> cSym
              <> "_grid(double centerRe, double centerIm, double scale,"
          , "    double w, double h, double blk, double bxN, double byN) {"
          , "  int nx = (int)bxN;"
          , "  int ny = (int)byN;"
          , "  if (nx <= 0 || ny <= 0 || nx * ny > JSHARK_GRID_CAP) { return 0; }"
          , "  double half = blk * 0.5;"
          , "  double invW = 1.0 / w;"
          , "  double invH = 1.0 / h;"
          , "  double halfW = w * 0.5;"
          , "  double halfH = h * 0.5;"
          , "  for (int by = 0; by < ny; by++) {"
          , "    double ci = centerIm + ((double)by * blk + half - halfH) * scale * invH;"
          , "    int32_t *row = &jshark_grid_buf[by * nx];"
          , "    int bx = 0;"
          , "#ifdef __wasm_simd128__"
          , "    for (; bx + 3 < nx; bx += 4) {"
          , "      double cr0 = centerRe + ((double)bx * blk + half - halfW) * scale * invW;"
          , "      double cr1 ="
          , "          centerRe + ((double)(bx + 1) * blk + half - halfW) * scale * invW;"
          , "      double cr2 ="
          , "          centerRe + ((double)(bx + 2) * blk + half - halfW) * scale * invW;"
          , "      double cr3 ="
          , "          centerRe + ((double)(bx + 3) * blk + half - halfW) * scale * invW;"
          , "      jshark_mandel_quad(cr0, cr1, cr2, cr3, ci, &row[bx]);"
          , "    }"
          , "#endif"
          , "    for (; bx < nx; bx++) {"
          , "      double cr = centerRe + ((double)bx * blk + half - halfW) * scale * invW;"
          , "      row[bx] = (int32_t)jshark_mandel_iter(cr, ci);"
          , "    }"
          , "  }"
          , "  return (int32_t)(uintptr_t)jshark_grid_buf;"
          , "}"
          , ""
          , "/* Same grid contract, but computed by HVM2 reducing the Bend-compiled"
          , " * jshark_grid def: interaction-net execution end to end (f24 math). */"
          , "__attribute__((export_name(\""
              <> bendId
              <> "_hvm2_grid\")))"
          , "int32_t jshark_export_"
              <> cSym
              <> "_hvm2_grid(double centerRe, double centerIm, double scale,"
          , "    double w, double h, double blk, double bxN, double byN) {"
          , "  int nx = (int)bxN;"
          , "  int ny = (int)byN;"
          , "  if (nx <= 0 || ny <= 0 || nx * ny > JSHARK_GRID_CAP) {"
          , "    jshark_hvm2_last_k = -4;"
          , "    return 0;"
          , "  }"
          , "  if (!jshark_hvm2_boot()) {"
          , "    if (jshark_hvm2_last_k == 0) { jshark_hvm2_last_k = -1; }"
          , "    return 0;"
          , "  }"
          , "  u32 fid = jshark_hvm2_def_id(\"jshark_grid\");"
          , "  if (fid == 0xFFFFFFFF) {"
          , "    jshark_hvm2_last_k = -2;"
          , "    return 0;"
          , "  }"
          , "  int fnx = 0;"
          , "  int fny = 0;"
          , "  int cells;"
          , "  int k;"
          , "  double blk2;"
          , "  double args[8];"
          , "  (void)blk;"
          , "  jshark_hvm2_fit_grid(nx, ny, &fnx, &fny);"
          , "  cells = fnx * fny;"
          , "  blk2 = w / (double)fnx;"
          , "  {"
          , "    double bh = h / (double)fny;"
          , "    if (bh < blk2) { blk2 = bh; }"
          , "  }"
          , "  args[0] = centerRe;"
          , "  args[1] = centerIm;"
          , "  args[2] = scale;"
          , "  args[3] = w;"
          , "  args[4] = h;"
          , "  args[5] = blk2;"
          , "  args[6] = (double)fnx;"
          , "  args[7] = (double)fny;"
          , "  k = jshark_hvm2_run_grid(fid, args, cells);"
          , "  if (k != cells) {"
          , "    jshark_hvm2_last_k = k;"
          , "    return 0;"
          , "  }"
          , "  jshark_hvm2_upsample(fnx, fny, nx, ny);"
          , "  jshark_hvm2_last_k = nx * ny;"
          , "  return (int32_t)(uintptr_t)jshark_grid_buf;"
          , "}"
          , ""
          , "__attribute__((export_name(\"jshark_hvm2_last_k\")))"
          , "int32_t jshark_export_hvm2_last_k(void) {"
          , "  return (int32_t)jshark_hvm2_last_k;"
          , "}"
          , ""
          ]

cIdent :: Text -> Text
cIdent t =
  let
    id_ = sanitizeBendId t
   in
    if id_ `elem` cKeywords
      then "k_" <> id_
      else id_

cKeywords :: [Text]
cKeywords =
  [ "auto"
  , "break"
  , "case"
  , "char"
  , "const"
  , "continue"
  , "default"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "extern"
  , "float"
  , "for"
  , "goto"
  , "if"
  , "inline"
  , "int"
  , "long"
  , "register"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "struct"
  , "switch"
  , "typedef"
  , "union"
  , "unsigned"
  , "void"
  , "volatile"
  , "while"
  ]
