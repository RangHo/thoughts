import adapter from '@sveltejs/adapter-static'

import { vitePreprocess } from '@sveltejs/vite-plugin-svelte'
import { sveltePreprocess } from 'svelte-preprocess'
import orgPreprocess from 'svelte-preprocess-org'

/** @type {import('@sveltejs/kit').Config} */
const config = {
  preprocess: [
    orgPreprocess({
      extensions: ['.org'],
      idLocations: ['src/posts/**/*.org'],
      latexEnvironmentFormat: '<Math expression={%s} display />',
      latexFragmentFormat: '<Math expression={%s} />',
      srcBlockFormat: '<Code lang={\'%s\'} code={%s} />',
      additionalComponents: [
        { binding: ['Math', 'Code'], module: '@rangho/ui' },
      ],
    }),
    sveltePreprocess(),
    vitePreprocess(),
  ],
  extensions: ['.svelte', '.org'],
  kit: {
    // Use static adapter to render individual pages as static HTML
    adapter: adapter({
      // GitHub Pages requires a custom 404.html
      fallback: '404.html',
    }),
    // Prerendering settings
    prerender: {
      handleHttpError: 'warn',
    },
    // Declare custom import aliases
    alias: {
      $posts: './src/posts',
    },
  },
}
export default config
