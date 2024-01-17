import { writable } from "svelte/store";
import { browser } from "$app/environment";

export type ColorMode = "light" | "dark";

let prefersColorScheme: MediaQueryList;

export function init() {
  if (!browser) {
    // Server-side rendering, do nothing
    return;
  }

  prefersColorScheme = window.matchMedia("(prefers-color-scheme: dark)");
  prefersColorScheme.addEventListener("change", (e) => {
    colorMode.set(e.matches ? "dark" : "light");
  });
}

export const colorMode = writable<ColorMode>("light");
