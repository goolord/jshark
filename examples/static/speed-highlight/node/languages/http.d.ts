declare const _default: ({
    type: "kwd";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    expand: "str";
    type?: undefined;
    match?: undefined;
    sub?: undefined;
} | {
    type: "section";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    expand: "num";
    type?: undefined;
    match?: undefined;
    sub?: undefined;
} | {
    type: "oper";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    type: "var";
    match: RegExp;
    expand?: undefined;
    sub?: undefined;
} | {
    match: RegExp;
    sub: typeof detectLanguage;
    type?: undefined;
    expand?: undefined;
})[];
export default _default;
import { detectLanguage } from '../detect.js';
//# sourceMappingURL=http.d.ts.map