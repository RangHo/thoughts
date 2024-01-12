<script lang="ts">
  import hljs from 'highlight.js';
  import he from 'he';

  import 'highlight.js/styles/hybrid.css';

  export let lang: string;
  export let code: string;
  export let inline = false;

  let highlighted = '';

  $: highlighted = hljs.highlight(he.decode(code), { language: lang }).value;
</script>

<svelte:head>
  <link
    rel="stylesheet"
    href="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/default.min.css"
  />
</svelte:head>

{#if inline}
  <code class="hljs language-{lang}">{@html highlighted}</code>
{:else}
  <pre class="my-4"><code class="hljs language-{lang}">{@html highlighted}</code></pre>
{/if}
