declare const _default: ({
    type: "deleted";
    match: RegExp;
} | {
    type: "insert";
    match: RegExp;
} | {
    type: "kwd";
    match: RegExp;
} | {
    type: "section";
    match: RegExp;
} | {
    match: RegExp;
    type: "cmnt";
    sub: string;
    expand?: undefined;
} | {
    expand: "str";
    match?: undefined;
    type?: undefined;
    sub?: undefined;
} | {
    type: "func";
    match: RegExp;
    sub?: undefined;
    expand?: undefined;
})[];
export default _default;
//# sourceMappingURL=git.d.ts.map