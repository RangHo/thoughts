<script lang="ts">
  import { onMount } from 'svelte';

  import { getComponent } from '$lib/post';

	import type { PageData } from './$types';

  import 'katex/dist/katex.min.css';

	export let data: PageData;

  let target = getComponent(data.slug);
</script>

<style lang="postcss">
  h1 {
    @apply text-4xl font-bold;
  }
</style>

<h1>{data.title}</h1>

{#await target}
  <p>loading...</p>
{:then target}
  <svelte:component this={target} />
{:catch error}
  <p>error: {error.message}</p>
{/await}
