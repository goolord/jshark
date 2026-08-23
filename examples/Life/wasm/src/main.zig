//! SIMD memory kernels for the Life JS engine (LUTGenerator.js).
//! All Conway stepping logic stays in JavaScript; these exports only
//! accelerate row clears and copies on wasm linear memory.

const std = @import("std");

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

/// Grow wasm memory to at least `need` bytes. Returns 0 on success.
export fn growTo(need: u32) i32 {
    return if (growToBytes(need)) 0 else -1;
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
