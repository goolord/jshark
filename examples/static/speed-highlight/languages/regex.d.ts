declare namespace _default {
    let type: "oper";
    let sub: ({
        match: RegExp;
        type: "cmnt";
        sub: string;
    } | {
        type: "num";
        match: RegExp;
        sub?: undefined;
    } | {
        type: "kwd";
        match: RegExp;
        sub?: undefined;
    } | {
        type: "var";
        match: RegExp;
        sub?: undefined;
    })[];
}
export default _default;
//# sourceMappingURL=regex.d.ts.map