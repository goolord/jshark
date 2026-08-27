/**
 * Find the tokens in the given code, yielding the name of every
 * language it needs and expecting it to be sent back
 *
 * @param {string} src The code
 * @param {string|ShjLanguageData} lang The language of the code, by name or given directly
 * @param {ShjTokenCallback} onToken The callback function
 * @param {ShjToken} [fallback] Type for the whole region if the language cannot
 * be resolved, so a sub that is not given keeps the type of the rule embedding it
 * @yields {string} The name of a language to resolve
 * @returns {Generator<string, void, ShjLanguageData|undefined>}
 */
export function tokenizer(src: string, lang: string | ShjLanguageData, onToken: ShjTokenCallback, fallback?: ShjToken): Generator<string, void, ShjLanguageData | undefined>;
/**
 * Find the tokens in the given code and call the given callback,
 * without loading anything: every language used has to be given by the caller
 *
 * @example
 * import json from '@speed-highlight/core/languages/json.js';
 * import { tokenizeWith } from '@speed-highlight/core/tokenize';
 * import process from 'node:process';
 *
 * tokenizeWith(src, json, (str, type) => process.stdout.write(str));
 *
 * @param {string} src The code
 * @param {string|ShjLanguageData} lang The language of the code
 * @param {ShjTokenCallback} onToken Called with the text and type of each token
 * @param {{ languages?: Record<string, ShjLanguageData> }} [opt={}] Customization options
 */
export function tokenizeWith(src: string, lang: string | ShjLanguageData, onToken: ShjTokenCallback, opt?: {
    languages?: Record<string, ShjLanguageData>;
}): void;
/**
 * Token types
 */
export type ShjToken = ("deleted" | "err" | "var" | "section" | "kwd" | "class" | "cmnt" | "insert" | "type" | "func" | "bool" | "num" | "oper" | "str" | "esc");
/**
 * A stateful object behaving like a RegExp, as the tokenizer
 * only relies on `lastIndex` and `exec` (a RegExp fits the shape)
 */
export type ShjMatcher = {
    lastIndex: number;
    exec: (str: string) => ({
        index: number;
        0: string;
    } | null);
};
/**
 * One rule: a pattern to tag with a token type, one of the shared
 * patterns reused by name, or a region to re-tokenize with another language
 */
export type ShjRule = {
    expand: ("num" | "str" | "strDouble");
    match?: undefined;
} | {
    match: ShjMatcher;
    type?: ShjToken;
    sub?: string | ShjGrammar | ((code: string) => string | ShjLanguageData);
    expand?: undefined;
};
/**
 * The rules of a language
 */
export type ShjGrammar = ShjRule[];
/**
 * A language: its grammar alone, or with the type
 * given to the text the grammar does not match
 */
export type ShjLanguageData = ShjGrammar | {
    type?: ShjToken;
    sub: ShjGrammar;
};
/**
 * Called with the text and type of every token found
 */
export type ShjTokenCallback = (text: string, token?: ShjToken) => void;
//# sourceMappingURL=tokenize.d.ts.map