declare const _default: ({
    type: "cmnt";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    expand: "str";
    type?: undefined;
    match?: undefined;
    sub?: undefined;
} | {
    expand: "num";
    type?: undefined;
    match?: undefined;
    sub?: undefined;
} | {
    type: "num";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    type: "kwd";
    match: RegExp;
    sub: {
        type: "func";
        match: RegExp;
    }[];
    expand?: undefined;
} | {
    type: "kwd";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    match: RegExp;
    type: "oper";
    expand?: undefined;
    sub?: undefined;
})[];
export default _default;
//# sourceMappingURL=asm.d.ts.map