const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    // TPC_L2=2 → 4 HVM2 worker threads inside the wasm module.
    const tpc_l2 = b.option(u32, "tpc-l2", "log2 of HVM2 thread count (TPC = 2^tpc_l2)") orelse 2;
    const tpc_flag = b.fmt("-DTPC_L2={d}", .{tpc_l2});

    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
        .abi = .musl,
        // atomics + shared memory are required for browser pthreads.
        .cpu_features_add = std.Target.wasm.featureSet(&.{
            .atomics,
            .simd128,
            .bulk_memory,
            .sign_ext,
            .nontrapping_fptoint,
            .mutable_globals,
            .multivalue,
            .extended_const,
            .reference_types,
        }),
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

    const include_dir = b.path("include");

    const mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        // Zig's wasm libc allocator lacks -fno-single-threaded; HVM2 worker
        // threads are spawned from JS (Web Workers + shared Memory import).
        .single_threaded = true,
    });
    mod.addIncludePath(include_dir);
    const c_flags = [_][]const u8{ "-std=c11", tpc_flag, "-msimd128" };
    mod.addCSourceFile(.{
        .file = .{ .cwd_relative = kernel_c },
        .flags = &c_flags,
    });
    mod.addCSourceFile(.{
        .file = .{ .cwd_relative = exports_c },
        .flags = &c_flags,
    });

    const exe = b.addExecutable(.{
        .name = "jshark-hvm2",
        .root_module = mod,
    });
    exe.entry = .disabled;
    exe.rdynamic = true;
    // Host creates shared Memory; every instance (main + workers) imports it.
    exe.import_memory = true;
    exe.export_memory = true;
    exe.shared_memory = true;
    exe.lto = .full;

    b.installArtifact(exe);
}
