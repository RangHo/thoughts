import adapter from "@sveltejs/adapter-static";
import { vitePreprocess } from "@sveltejs/vite-plugin-svelte";
import { importAssets } from 'svelte-preprocess-import-assets';

/** @type {import('@sveltejs/kit').Config} */
const config = {
  preprocess: [
    vitePreprocess(),
    importAssets()
  ],

  kit: {
    // Use static adapter to render individual pages as static HTML
    adapter: adapter({
      // GitHub Pages requires a custom 404.html
      fallback: "404.html",
    }),

    // Prerendering settings
    prerender: {
      handleHttpError: 'warn',
    },

    // Declare custom import aliases
    alias: {
      $posts: "./src/posts",
    },
  },
};

export default config;
