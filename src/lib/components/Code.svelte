<script lang="ts">
  import hljs from 'highlight.js';
  import he from 'he';
  import lightTheme from 'highlight.js/styles/tokyo-night-light.css?inline';
  import darkTheme from 'highlight.js/styles/tokyo-night-dark.css?inline';

  import { colorMode } from '$lib/dark';

  const hljsThemes = {
    light: lightTheme,
    dark: darkTheme,
  };

  export let lang: string;
  export let code: string;
  export let inline = false;

  let highlighted = '';

  $: highlighted = hljs.highlight(he.decode(code), { language: lang }).value;
</script>

{@html `<style>${hljsThemes[$colorMode]}</style>`}

{#if inline}
  <code class="hljs language-{lang} font-monospace">{@html highlighted}</code>
{:else}
  <pre class="my-4"><code class="hljs language-{lang} font-monospace">{@html highlighted}</code></pre>
{/if}
