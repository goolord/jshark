const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
        .abi = .none,
    });

    const kernel_c = b.option(
        []const u8,
        "kernel-c",
        "absolute path to Bend/HVM2-generated kernel.c",
    ) orelse "generated/kernel.c";

    const exports_c = b.option(
        []const u8,
        "exports-c",
        "absolute path to JShark-generated kernel_exports.c",
    ) orelse "generated/kernel_exports.c";

    const mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    mod.addCSourceFile(.{
        .file = .{ .cwd_relative = kernel_c },
        .flags = &.{
            "-std=c11",
        },
    });
    mod.addCSourceFile(.{
        .file = .{ .cwd_relative = exports_c },
        .flags = &.{
            "-std=c11",
        },
    });

    const exe = b.addExecutable(.{
        .name = "jshark-hvm2",
        .root_module = mod,
    });
    exe.entry = .disabled;
    exe.rdynamic = true;
    exe.export_memory = true;

    b.installArtifact(exe);
}
