export as namespace toy;

export const VERSION: string;

export function greet(name: string): string;

export function add(a: number, b: number): number;

export function log(msg: string): void;

export interface Size {
  width: number;
  height: number;
}

export declare class Widget {
  constructor(id: string);
  readonly id: string;
  width: number;
  resize(w: number, h: number): void;
  size(): Size;
}

export declare namespace util {
  function clamp(n: number, lo: number, hi: number): number;
}
