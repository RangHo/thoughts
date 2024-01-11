<script lang="ts">
  import renderMathInElement from 'katex/contrib/auto-render';

  const katexOptions = {
    // pass
  };
</script>

<svelte:head>
  <link
    rel="stylesheet"
    href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css"
    integrity="sha384-n8MVd4RsNIU0tAv4ct0nTaAbDJwPJzDEaqSD1odI+WdtXRGWt2kTvGFasHpSy3SV"
    crossorigin="anonymous"
  />
</svelte:head>

<svelte:window on:load={() => renderMathInElement(document.body, katexOptions)} />

<slot />
