// Reads the imported memory limits out of a wasm binary, so harnesses do not
// hardcode a page count that changes whenever static storage changes.

function leb(u8, s) {
  let r = 0;
  let sh = 0;
  let b;
  do {
    b = u8[s.o++];
    r |= (b & 0x7f) << sh;
    sh += 7;
  } while (b & 0x80);
  return r;
}

export function importedMemoryLimits(bytes) {
  const u8 = bytes instanceof Uint8Array ? bytes : new Uint8Array(bytes);
  const s = { o: 8 };
  while (s.o < u8.length) {
    const id = leb(u8, s);
    const size = leb(u8, s);
    const end = s.o + size;
    if (id === 2) {
      const count = leb(u8, s);
      for (let i = 0; i < count; i++) {
        const modLen = leb(u8, s);
        s.o += modLen;
        const fieldLen = leb(u8, s);
        s.o += fieldLen;
        const kind = u8[s.o++];
        if (kind === 0x02) {
          const flags = u8[s.o++];
          const initial = leb(u8, s);
          const maximum = flags & 0x01 ? leb(u8, s) : initial;
          return { initial, maximum, shared: !!(flags & 0x02) };
        }
        if (kind === 0x00) {
          leb(u8, s);
        } else if (kind === 0x01) {
          s.o++;
          const f = u8[s.o++];
          leb(u8, s);
          if (f & 0x01) leb(u8, s);
        } else if (kind === 0x03) {
          s.o += 2;
        }
      }
    }
    s.o = end;
  }
  throw new Error('no imported memory found in wasm');
}
