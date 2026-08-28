/**
 * Replace how language names are loaded, call it before highlighting
 *
 * @example
 * setLoader(name => customs[name] ?? defaultLoader(name));
 *
 * @param {ShjLanguageLoader} newLoader Given a name, returns the language, its module, or a promise of either
 */
export function setLoader(newLoader: ShjLanguageLoader): void;
/**
 * Find the tokens in the given code and call the given callback,
 * bundled languages are loaded on first use
 *
 * @param {string} src The code
 * @param {ShjLanguage|ShjLanguageData} lang The language of the code
 * @param {ShjTokenCallback} onToken Called with the text and type of each token
 * @returns {Promise<void>} Resolves once every token has been emitted
 */
export function tokenize(src: string, lang: ShjLanguage | ShjLanguageData, onToken: ShjTokenCallback): Promise<void>;
/**
 * Highlight a string passed as argument and return it
 * @example
 * elm.innerHTML = await highlightHTML(code, 'js');
 *
 * @param {string} src The code
 * @param {ShjLanguage|ShjLanguageData} lang The language of the code
 * @param {ShjOptions} [opt={}] Customization options
 * @returns {Promise<string>} The highlighted string
 */
export function highlightHTML(src: string, lang: ShjLanguage | ShjLanguageData, opt?: ShjOptions): Promise<string>;
/**
 * Highlight a DOM element by getting the new innerHTML with highlightHTML
 *
 * @param {Element} elm The DOM element
 * @param {ShjLanguage} [lang] The language of the code (searching by default on `elm` for a 'shj-lang-' class)
 * @param {ShjOptions} [opt={}] Customization options, `block` defaults to the element: a `code` element is inline
 * @returns {Promise<void>} Resolves once the element has been highlighted
 */
export function highlightElement(elm: Element, lang?: ShjLanguage, opt?: ShjOptions): Promise<void>;
/**
 * Call highlightElement on element with a css class starting with `shj-lang-`
 *
 * @param {ShjOptions} [opt={}] Customization options
 * @returns {Promise<void[]>} Resolves once every element has been highlighted
 */
export function highlightAll(opt?: ShjOptions): Promise<void[]>;
/**
 * Highlight a string passed as argument and return a string that can directly
 * be printed in a terminal, bundled languages are loaded on first use
 *
 * @param {string} src The code
 * @param {ShjLanguage|ShjLanguageData} lang The language of the code
 * @param {ShjTerminalTheme} theme The theme to use, e.g. imported from `themes/atom-dark.js`
 * @returns {Promise<string>} The highlighted string
 */
export function highlightANSI(src: string, lang: ShjLanguage | ShjLanguageData, theme: ShjTerminalTheme): Promise<string>;
/**
 * Loader of the bundled languages, can be called
 * by a custom loader as its fallback
 *
 * @type {ShjLanguageLoader}
 */
export const defaultLoader: ShjLanguageLoader;
/**
 * Languages bundled by default
 */
export type ShjBuiltinLanguage = ("asm" | "bash" | "bf" | "c" | "css" | "csv" | "diff" | "docker" | "git" | "go" | "html" | "http" | "ini" | "java" | "js" | "jsdoc" | "json" | "leanpub-md" | "log" | "lua" | "make" | "md" | "pl" | "plain" | "py" | "regex" | "rs" | "sql" | "todo" | "toml" | "ts" | "uri" | "xml" | "yaml");
/**
 * A bundled language or any name the loader can give
 */
export type ShjLanguage = ShjBuiltinLanguage | (string & {});
/**
 * Republished from `tokenize.js` so writing a custom language only
 * takes the main entry, even for the types it never mentions itself
 */
export type ShjToken = import("./tokenize.js").ShjToken;
/**
 * Republished from `tokenize.js` so writing a custom language only
 * takes the main entry, even for the types it never mentions itself
 */
export type ShjMatcher = import("./tokenize.js").ShjMatcher;
/**
 * Republished from `tokenize.js` so writing a custom language only
 * takes the main entry, even for the types it never mentions itself
 */
export type ShjLanguageData = import("./tokenize.js").ShjLanguageData;
/**
 * Republished from `tokenize.js` so writing a custom language only
 * takes the main entry, even for the types it never mentions itself
 */
export type ShjRule = import("./tokenize.js").ShjRule;
/**
 * Republished from `tokenize.js` so writing a custom language only
 * takes the main entry, even for the types it never mentions itself
 */
export type ShjGrammar = import("./tokenize.js").ShjGrammar;
/**
 * Republished from `tokenize.js` so writing a custom language only
 * takes the main entry, even for the types it never mentions itself
 */
export type ShjTokenCallback = import("./tokenize.js").ShjTokenCallback;
/**
 * Give a language for the asked name: the language, its module, or a promise of either
 */
export type ShjLanguageLoader = (name: string) => ShjLanguageData | {
    default: ShjLanguageData;
} | Promise<ShjLanguageData | {
    default: ShjLanguageData;
}> | undefined;
/**
 * Themes supported in the browser
 */
export type ShjBrowserTheme = ("atom-dark" | "github-dark" | "github-dim" | "dark" | "default" | "github-light" | "visual-studio-dark");
/**
 * A theme, mapping each token type to the ANSI escape printed before it
 */
export type ShjTerminalTheme = Partial<Record<ShjToken, string>>;
export type ShjOptions = {
    /**
     * Render as a block, with the line numbering
     * and header wrapper, rather than inline. `highlightElement` defaults it from
     * the element instead: a `code` element is inline, anything else is a block
     */
    block?: boolean;
    /**
     * Indicates whether to number the
     * lines, in a gutter laid out inside the block
     */
    showLineNumbers?: boolean;
};
//# sourceMappingURL=index.d.ts.map