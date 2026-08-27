declare namespace _default {
    let type: "cmnt";
    let sub: ({
        type: "err";
        match: RegExp;
    } | {
        type: "class";
        match: RegExp;
    } | {
        type: "insert";
        match: RegExp;
    } | {
        type: "oper";
        match: RegExp;
    })[];
}
export default _default;
//# sourceMappingURL=todo.d.ts.map