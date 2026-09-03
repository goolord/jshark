//! SIMD memory kernels and LUT stepping for the Life JS engine.
//! Conway logic matches LUTGenerator.js; wasm removes JS loop overhead.

const std = @import("std");

const lut_size: u32 = 65536;

fn growToBytes(need: u32) bool {
    const page: u32 = 65536;
    const pages = (need + page - 1) / page;
    const cur = @wasmMemorySize(0);
    if (pages <= cur) return true;
    return @wasmMemoryGrow(0, pages - cur) != std.math.maxInt(u32);
}

fn memoryBytes() u32 {
    return @wasmMemorySize(0) * 65536;
}

fn rangeInBounds(off: u32, len: u32) bool {
    if (len == 0) return true;
    const end = off +% len;
    return end >= off and end <= memoryBytes();
}

fn memAt(off: u32) [*]u8 {
    return @ptrFromInt(off);
}

fn clearRowSimd(dst: [*]u8, len: u32) void {
    var i: u32 = 0;
    const Vec16 = @Vector(16, u8);
    const zero: Vec16 = @splat(0);
    while (i + 16 <= len) : (i += 16) {
        @as(*Vec16, @alignCast(@ptrCast(dst + i))).* = zero;
    }
    while (i < len) : (i += 1) dst[i] = 0;
}

fn copyRowSimd(src: [*]const u8, dst: [*]u8, len: u32) void {
    var i: u32 = 0;
    const Vec16 = @Vector(16, u8);
    while (i + 16 <= len) : (i += 16) {
        const chunk = @as(*const Vec16, @alignCast(@ptrCast(src + i))).*;
        @as(*Vec16, @alignCast(@ptrCast(dst + i))).* = chunk;
    }
    while (i < len) : (i += 1) dst[i] = src[i];
}

fn computeNextByte(
    top: u8,
    cur: u8,
    bot: u8,
    lt: u8,
    lc: u8,
    lb: u8,
    rt: u8,
    rc: u8,
    rb: u8,
) u8 {
    var out: u8 = 0;
    var bit: u32 = 0;
    while (bit < 8) : (bit += 1) {
        const b: u3 = @intCast(bit);
        const alive = (cur >> b) & 1;
        const left = if (bit > 0) (cur >> (b - 1)) & 1 else lc;
        const right = if (bit < 7) (cur >> (b + 1)) & 1 else rc;
        const top_l = if (bit > 0) (top >> (b - 1)) & 1 else lt;
        const top_c = (top >> b) & 1;
        const top_r = if (bit < 7) (top >> (b + 1)) & 1 else rt;
        const bot_l = if (bit > 0) (bot >> (b - 1)) & 1 else lb;
        const bot_c = (bot >> b) & 1;
        const bot_r = if (bit < 7) (bot >> (b + 1)) & 1 else rb;
        const n = top_l + top_c + top_r + left + right + bot_l + bot_c + bot_r;
        const next = if (alive != 0) n == 2 or n == 3 else n == 3;
        if (next) out |= @as(u8, 1) << b;
    }
    return out;
}

fn stepChunk(
    lut: [*]const u8,
    top: u8,
    cur: u8,
    bot: u8,
    lt: u8,
    lc: u8,
    lb: u8,
    rt: u8,
    rc: u8,
    rb: u8,
) u8 {
    const edge = lt | lc | lb | rt | rc | rb;
    if ((top | cur | bot | edge) == 0) return 0;
    if (bot == 0 and edge == 0) {
        const key = (@as(u16, top) << 8) | cur;
        return lut[key];
    }
    return computeNextByte(top, cur, bot, lt, lc, lb, rt, rc, rb);
}

fn packCells(row: [*]const u8, w: u32, x0: u32) u8 {
    var byte: u8 = 0;
    var b: u32 = 0;
    while (b < 8) : (b += 1) {
        const x = x0 + b;
        if (x >= w) continue;
        if (row[x] & 1 != 0) byte |= @as(u8, 1) << @intCast(b);
    }
    return byte;
}

fn unpackCells(row: [*]u8, w: u32, x0: u32, byte: u8) void {
    var b: u32 = 0;
    while (b < 8) : (b += 1) {
        const x = x0 + b;
        if (x >= w) continue;
        row[x] = if ((byte >> @intCast(b)) & 1 != 0) 1 else 0;
    }
}

fn edgeBit(row: [*]const u8, w: u32, col: i32) u8 {
    if (col < 0 or @as(u32, @intCast(col)) >= w) return 0;
    return if (row[@intCast(col)] & 1 != 0) 1 else 0;
}

fn stepRegionRows(
    lut: [*]const u8,
    grid_a: [*]const u8,
    grid_b: [*]u8,
    w: u32,
    h: u32,
    y0: u32,
    y1: u32,
) void {
    const y_start = @max(@as(u32, 1), y0);
    const y_stop = @min(h -| 1, y1);
    var y: u32 = y_start;
    while (y < y_stop) : (y += 1) {
        const top_off = (y - 1) * w;
        const cur_off = y * w;
        const bot_off = (y + 1) * w;
        clearRowSimd(grid_b + cur_off, w);
        const bytes = (w + 7) / 8;
        var xb: u32 = 0;
        while (xb < bytes) : (xb += 1) {
            const x0 = xb * 8;
            if (x0 >= w) continue;
            const left_col: i32 = @as(i32, @intCast(x0)) - 1;
            const right_col: i32 = @as(i32, @intCast(x0)) + 8;
            const lt = edgeBit(grid_a + top_off, w, left_col);
            const lc = edgeBit(grid_a + cur_off, w, left_col);
            const lb = edgeBit(grid_a + bot_off, w, left_col);
            const rt = edgeBit(grid_a + top_off, w, right_col);
            const rc = edgeBit(grid_a + cur_off, w, right_col);
            const rb = edgeBit(grid_a + bot_off, w, right_col);
            const top = packCells(grid_a + top_off, w, x0);
            const cur = packCells(grid_a + cur_off, w, x0);
            const bot = packCells(grid_a + bot_off, w, x0);
            if ((top | cur | bot | lt | lc | lb | rt | rc | rb) == 0) continue;
            const next = stepChunk(lut, top, cur, bot, lt, lc, lb, rt, rc, rb);
            unpackCells(grid_b + cur_off, w, x0, next);
        }
    }
    if (y0 == 0) copyRowSimd(grid_a, grid_b, w);
    if (y1 >= h) {
        const bot_off = (h - 1) * w;
        copyRowSimd(grid_a + bot_off, grid_b + bot_off, w);
    }
}

/// Grow wasm memory to at least `need` bytes. Returns 0 on success.
export fn growTo(need: u32) i32 {
    return if (growToBytes(need)) 0 else -1;
}

/// Build the 65536-entry LUT at `lutOff` (matches JS createLifeLUT).
export fn initLUT(lutOff: u32) void {
    if (!rangeInBounds(lutOff, lut_size)) return;
    const lut = memAt(lutOff);
    var key: u32 = 0;
    while (key < lut_size) : (key += 1) {
        const top = @as(u8, @truncate((key >> 8) & 0xff));
        const cur = @as(u8, @truncate(key & 0xff));
        lut[key] = computeNextByte(top, cur, 0, 0, 0, 0, 0, 0, 0);
    }
}

/// Step rows [y0, y1) with the LUT chunking algorithm.
export fn stepRegionLUT(
    lutOff: u32,
    aOff: u32,
    bOff: u32,
    w: u32,
    h: u32,
    y0: u32,
    y1: u32,
) void {
    if (w == 0 or h == 0) return;
    const grid_bytes = w * h;
    if (!rangeInBounds(lutOff, lut_size)) return;
    if (!rangeInBounds(aOff, grid_bytes)) return;
    if (!rangeInBounds(bOff, grid_bytes)) return;
    stepRegionRows(
        memAt(lutOff),
        memAt(aOff),
        memAt(bOff),
        w,
        h,
        y0,
        y1,
    );
}

/// Zero `len` bytes at `offset` in linear memory (SIMD when aligned).
export fn clearRow(offset: u32, len: u32) void {
    if (!rangeInBounds(offset, len)) return;
    clearRowSimd(memAt(offset), len);
}

/// Copy `len` bytes from `src` to `dst` in linear memory (SIMD when aligned).
export fn copyRow(src: u32, dst: u32, len: u32) void {
    if (!rangeInBounds(src, len) or !rangeInBounds(dst, len)) return;
    copyRowSimd(@ptrFromInt(src), memAt(dst), len);
}
