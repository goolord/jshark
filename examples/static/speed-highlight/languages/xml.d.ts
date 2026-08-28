export let name: string;
export let properties: string;
/** @type {{ match: RegExp, sub: import('../index.js').ShjGrammar }} */
export let xmlElement: {
    match: RegExp;
    sub: import("../index.js").ShjGrammar;
};
declare const _default: ({
    match: RegExp;
    sub: import("../index.js").ShjGrammar;
} | {
    match: RegExp;
    type: "cmnt";
    sub: string;
} | {
    type: "class";
    match: RegExp;
    sub?: undefined;
} | {
    type: "str";
    match: RegExp;
    sub: ({
        type: "var";
        match: RegExp;
        sub: {
            type: "oper";
            match: RegExp;
        }[];
    } | {
        type: "oper";
        match: RegExp;
        sub?: undefined;
    })[];
} | {
    type: "var";
    match: RegExp;
    sub?: undefined;
})[];
export default _default;
//# sourceMappingURL=xml.d.ts.map