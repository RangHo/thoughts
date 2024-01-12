<script lang="ts">
  import katex from 'katex';

  export let expression = '';
  export let display = false;
</script>

{#if expression}
  {@html katex.renderToString(expression, { displayMode: display })}
{/if}
