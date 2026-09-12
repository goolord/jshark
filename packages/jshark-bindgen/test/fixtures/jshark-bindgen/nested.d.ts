// Nested nullability: the emitter adapts a top-level `T | null` argument,
// but a nullable inside an array or callback is not yet converted, so the
// extractor surfaces an unsupported-nullable diagnostic.

export function sumWithNull(xs: Array<number | null>): number;

export function useNullableCallback(cb: (x: number | null) => void): void;
