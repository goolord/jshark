declare const _default: ({
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
    expand: "num";
    match?: undefined;
    type?: undefined;
    sub?: undefined;
} | {
    type: "kwd";
    match: RegExp;
    sub: {
        type: "str";
        match: RegExp;
    }[];
    expand?: undefined;
} | {
    match: RegExp;
    sub: ({
        type: "kwd";
        match: RegExp;
        sub?: undefined;
    } | {
        match: RegExp;
        sub: string;
        type?: undefined;
    })[];
    type?: undefined;
    expand?: undefined;
} | {
    type: "kwd";
    match: RegExp;
    sub?: undefined;
    expand?: undefined;
} | {
    type: "oper";
    match: RegExp;
    sub?: undefined;
    expand?: undefined;
} | {
    type: "func";
    match: RegExp;
    sub?: undefined;
    expand?: undefined;
} | {
    type: "class";
    match: RegExp;
    sub?: undefined;
    expand?: undefined;
})[];
export default _default;
//# sourceMappingURL=c.d.ts.map