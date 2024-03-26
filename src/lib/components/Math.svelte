<script lang="ts">
  import katex from 'katex';
  import 'katex/dist/katex.min.css';

  export let expr = '';
  export let display = false;

  const katexOptions: katex.KatexOptions = {
    displayMode: display,
  };
</script>

{#if expr}
  {@html katex.renderToString(expr, katexOptions)}
{/if}

<style lang="postcss">
  /* Fix vertical overflow when horizontal overflow is present
      (See https://github.com/KaTeX/KaTeX/discussions/2942) */
  :global(.katex-display) {
    @apply overflow-x-auto overflow-y-hidden;
  }
</style>
