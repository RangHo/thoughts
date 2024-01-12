<script lang="ts">
  import { onMount } from 'svelte';
  import katex from 'katex';
  import 'katex/dist/katex.min.css';

  export let expression = '';
  export let display = false;

  const katexOptions: katex.KatexOptions = {
    displayMode: display,
  };
</script>

{#if expression}
  {@html katex.renderToString(expression, katexOptions)}
{/if}

<style lang="postcss">
  :global(.katex-display) {
    @apply overflow-x-auto overflow-y-hidden;
  }
</style>
