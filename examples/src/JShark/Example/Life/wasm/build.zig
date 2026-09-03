const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
            .abi = .none,
            .cpu_features_add = std.Target.wasm.featureSet(&.{.simd128}),
        }),
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "life-simd",
        .root_module = mod,
    });
    exe.entry = .disabled;
    exe.rdynamic = true;
    exe.export_memory = true;

    b.installArtifact(exe);
}
