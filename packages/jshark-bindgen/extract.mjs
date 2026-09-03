#!/usr/bin/env bun
// TypeScript compiler API → jshark-bindgen IR JSON.
// Usage: bun extract.mjs [--module NAME] [--prefix NAME] FILE

import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createRequire } from "node:module";

const here = dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);

function loadTypescript() {
  const local = resolve(here, "node_modules/typescript");
  try {
    if (existsSync(local)) return require(local);
    return require("typescript");
  } catch {
    console.error(
      "jshark-bindgen: install typescript next to extract.mjs:\n  cd jshark-bindgen && bun install",
    );
    process.exit(2);
  }
}

const ts = loadTypescript();

function parseArgs(argv) {
  let moduleName = "";
  let prefix = "";
  let file = "";
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--module" && argv[i + 1]) moduleName = argv[++i];
    else if (a === "--prefix" && argv[i + 1]) prefix = argv[++i];
    else if (!a.startsWith("-")) file = a;
  }
  if (!file) {
    console.error("extract.mjs: missing FILE");
    process.exit(1);
  }
  return { moduleName, prefix, file: resolve(file) };
}

function tag(k, extra = {}) {
  return { k, ...extra };
}

function qualFfi(ffiPrefix, name) {
  if (!ffiPrefix) return name;
  if (name === ffiPrefix) return name;
  if (name.startsWith(`${ffiPrefix}.`)) return name;
  return `${ffiPrefix}.${name}`;
}

function serializeType(checker, type) {
  if (!type) return tag("unk", { note: "missing" });
  const f = ts.TypeFlags;
  if (type.flags & f.Void || type.flags & f.Undefined || type.flags & f.Never) {
    return tag("unit");
  }
  if (type.flags & f.Null) return tag("unit");
  if (type.flags & f.Number || type.flags & f.NumberLiteral) return tag("num");
  if (type.flags & f.BigInt || type.flags & f.BigIntLiteral) return tag("bigint");
  if (type.flags & f.String || type.flags & f.StringLiteral) return tag("str");
  if (type.flags & f.Boolean || type.flags & f.BooleanLiteral) return tag("bool");
  if (type.flags & f.ESSymbol) return tag("unk", { note: "symbol" });
  if (type.flags & f.Any || type.flags & f.Unknown) {
    return tag("unk", { note: checker.typeToString(type) });
  }
  if (type.isUnion()) {
    const parts = type.types.map((t) => serializeType(checker, t));
    return foldUnion(parts);
  }
  if (typeof checker.isArrayType === "function" && checker.isArrayType(type)) {
    const el = type.typeArguments?.[0] ?? type.resolvedTypeArguments?.[0];
    return tag("arr", { el: serializeType(checker, el) });
  }
  const name = type.symbol?.getName?.() ?? type.intrinsicName ?? "";
  const args = type.aliasTypeArguments ?? type.typeArguments ?? [];
  const lower = String(name).toLowerCase();
  if (lower === "uint8array" || lower === "uint8clampedarray") return tag("u8");
  if (lower === "promise") {
    return tag("promise", { el: serializeType(checker, args[0]) });
  }
  if (lower === "array" || lower === "readonlyarray") {
    return tag("arr", { el: serializeType(checker, args[0]) });
  }
  if (lower === "map") {
    return tag("map", {
      key: serializeType(checker, args[0]),
      val: serializeType(checker, args[1]),
    });
  }
  if (lower === "set") {
    return tag("set", { el: serializeType(checker, args[0]) });
  }
  const calls = type.getCallSignatures?.() ?? [];
  if (calls.length && !type.isClassOrInterface?.()) {
    const sig = calls[0];
    return tag("fn", {
      args: sig.getParameters().map((p) => {
        const pt = checker.getTypeOfSymbolAtLocation(
          p,
          p.valueDeclaration ?? type.symbol?.valueDeclaration,
        );
        return serializeType(checker, pt);
      }),
      ret: serializeType(checker, sig.getReturnType()),
    });
  }
  if (name && name !== "__type" && name !== "__object") {
    return tag("named", { n: name });
  }
  return tag("unk", { note: checker.typeToString(type) });
}

function foldUnion(parts) {
  const nullish = (p) => p.k === "unit";
  const core = parts.filter((p) => !nullish(p));
  const hasNull = parts.some(nullish);
  if (core.length === 0) return tag("unit");
  if (core.length === 1) return hasNull ? tag("opt", { el: core[0] }) : core[0];
  if (core.every((p) => p.k === "str")) return tag("str");
  if (core.every((p) => p.k === "num")) return tag("num");
  if (core.every((p) => p.k === "bool")) return tag("bool");
  return tag("unk", { note: "union " + core.map((p) => p.k).join("|") });
}

function paramOf(checker, p) {
  const decl = p.valueDeclaration;
  const ty = decl
    ? checker.getTypeOfSymbolAtLocation(p, decl)
    : checker.getDeclaredTypeOfSymbol(p);
  const optional = !!(p.flags & ts.SymbolFlags.Optional);
  return {
    name: p.getName(),
    ty: serializeType(checker, ty),
    optional,
  };
}

function funOf(checker, name, ffi, sig, ctor, isStatic = false) {
  return {
    name,
    ffi,
    params: sig.getParameters().map((p) => paramOf(checker, p)),
    ret: serializeType(checker, sig.getReturnType()),
    ctor: !!ctor,
    static: !!isStatic,
  };
}

function collectClass(checker, sym, ffi) {
  const t = checker.getDeclaredTypeOfSymbol(sym);
  const ctorT =
    (checker.getTypeOfSymbol && checker.getTypeOfSymbol(sym))
    || (sym.valueDeclaration
      ? checker.getTypeOfSymbolAtLocation(sym, sym.valueDeclaration)
      : t);
  const props = [];
  const methods = [];
  const ctors = [];
  const ctorSigs = [
    ...(ctorT.getConstructSignatures?.() ?? []),
    ...(t.getConstructSignatures?.() ?? []),
  ];
  const seen = new Set();
  for (const sig of ctorSigs) {
    const key = sig.getParameters().map((p) => p.getName()).join(",");
    if (seen.has(key)) continue;
    seen.add(key);
    ctors.push(funOf(checker, sym.getName(), ffi, sig, true));
  }
  for (const mem of t.getProperties?.() ?? []) {
    const decl = mem.valueDeclaration ?? mem.declarations?.[0];
    if (!decl) continue;
    const mt = checker.getTypeOfSymbolAtLocation(mem, decl);
    const sigs = mt.getCallSignatures?.() ?? [];
    const ro = !!(
      ts.getCombinedModifierFlags(decl) & ts.ModifierFlags.Readonly
    );
    if (sigs.length) {
      const isStatic = !!(mem.flags & ts.SymbolFlags.Static);
      methods.push(
        funOf(checker, mem.getName(), mem.getName(), sigs[0], false, isStatic),
      );
    } else {
      props.push({
        name: mem.getName(),
        ty: serializeType(checker, mt),
        readonly: ro,
      });
    }
  }
  return {
    name: sym.getName(),
    ffi,
    ctors,
    props,
    methods,
  };
}

function collectEnum(checker, sym) {
  const members = [];
  const t = checker.getDeclaredTypeOfSymbol(sym);
  for (const mem of t.getProperties?.() ?? []) {
    const decl = mem.valueDeclaration;
    let value = null;
    let numeric = false;
    if (decl && ts.isEnumMember(decl) && decl.initializer) {
      if (ts.isStringLiteral(decl.initializer)) {
        value = decl.initializer.text;
      } else if (ts.isNumericLiteral(decl.initializer)) {
        value = decl.initializer.text;
        numeric = true;
      }
    }
    members.push({ name: mem.getName(), value, numeric });
  }
  return { name: sym.getName(), members };
}

function walkSymbol(checker, sym, ffiPrefix, into) {
  const name = sym.getName();
  if (name === "default" || name.startsWith("__")) return;
  const flags = sym.getFlags();
  const ffi = qualFfi(ffiPrefix, name);
  if (flags & ts.SymbolFlags.Alias) {
    const aliased = checker.getAliasedSymbol(sym);
    walkSymbol(checker, aliased, ffiPrefix, into);
    return;
  }
  if (flags & (ts.SymbolFlags.Class | ts.SymbolFlags.Interface)) {
    into.classes.push(collectClass(checker, sym, ffi));
    return;
  }
  if (flags & ts.SymbolFlags.Function) {
    const t = checker.getTypeOfSymbolAtLocation(
      sym,
      sym.valueDeclaration ?? sym.declarations?.[0],
    );
    const sigs = t.getCallSignatures();
    if (sigs[0]) into.funs.push(funOf(checker, name, ffi, sigs[0], false));
    return;
  }
  if (flags & ts.SymbolFlags.Enum) {
    into.enums.push(collectEnum(checker, sym));
    return;
  }
  if (flags & (ts.SymbolFlags.ValueModule | ts.SymbolFlags.Namespace)) {
    const exports = checker.getExportsOfModule(sym);
    for (const e of exports) walkSymbol(checker, e, ffi, into);
    return;
  }
  if (flags & ts.SymbolFlags.Variable) {
    const decl = sym.valueDeclaration ?? sym.declarations?.[0];
    const t = decl
      ? checker.getTypeOfSymbolAtLocation(sym, decl)
      : checker.getDeclaredTypeOfSymbol(sym);
    const sigs = t.getCallSignatures?.() ?? [];
    if (sigs[0] && !(flags & ts.SymbolFlags.Interface)) {
      into.funs.push(funOf(checker, name, ffi, sigs[0], false));
    } else {
      into.consts.push({
        name,
        ffi,
        ty: serializeType(checker, t),
      });
    }
    return;
  }
  into.skipped.push({ name, reason: "unsupported symbol" });
}

function moduleFromFile(file) {
  const base = file.split("/").pop().replace(/\.(d\.)?(ts|tsx|js|mjs|cjs)$/i, "");
  const titled = base ? base[0].toUpperCase() + base.slice(1) : "Bindings";
  return "JShark." + titled;
}

function main() {
  const { moduleName, prefix, file } = parseArgs(process.argv.slice(2));
  const options = {
    allowJs: true,
    checkJs: true,
    declaration: true,
    noEmit: true,
    skipLibCheck: true,
    moduleResolution: ts.ModuleResolutionKind.Bundler,
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.ESNext,
  };
  const program = ts.createProgram([file], options);
  const checker = program.getTypeChecker();
  const sf = program.getSourceFile(file);
  if (!sf) {
    console.error("extract.mjs: could not load " + file);
    process.exit(1);
  }
  const into = {
    module: moduleName || moduleFromFile(file),
    prefix: prefix,
    source: file,
    classes: [],
    funs: [],
    consts: [],
    enums: [],
    skipped: [],
  };
  for (const stmt of sf.statements) {
    if (ts.isNamespaceExportDeclaration(stmt) && stmt.name) {
      into.prefix = into.prefix || stmt.name.text;
    }
  }
  const ns = into.prefix;
  const mod = checker.getSymbolAtLocation(sf);
  if (mod) {
    for (const e of checker.getExportsOfModule(mod)) {
      walkSymbol(checker, e, ns, into);
    }
  }
  // Script / UMD: also walk top-level statements.
  for (const stmt of sf.statements) {
    if (
      ts.isFunctionDeclaration(stmt) &&
      stmt.name &&
      !into.funs.some((f) => f.name === stmt.name.text)
    ) {
      const sym = checker.getSymbolAtLocation(stmt.name);
      if (sym) walkSymbol(checker, sym, ns, into);
    }
    if (
      (ts.isClassDeclaration(stmt) || ts.isInterfaceDeclaration(stmt)) &&
      stmt.name
    ) {
      const sym = checker.getSymbolAtLocation(stmt.name);
      if (sym && !into.classes.some((c) => c.name === stmt.name.text)) {
        walkSymbol(checker, sym, ns, into);
      }
    }
  }
  process.stdout.write(JSON.stringify(into));
}

main();
