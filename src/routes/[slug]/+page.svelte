<script lang="ts">
  import { onMount } from 'svelte';

  import { getComponent } from '$lib/post';

	import type { PageData } from './$types';

  import './styles.postcss';

	export let data: PageData;

  let target = getComponent(data.slug);
</script>

<header>
  <hgroup
    class="pt-2 pb-4 mx-auto max-w-4xl bg-white dark:bg-gray-900 border-b-2 border-gray-200 dark:border-gray-700 text-center sm:text-left"
  >
    <h1
      class="pb-2 text-3xl lg:text-4xl text-gray-900 dark:text-white font-heading font-bold"
    >
      {data.title}
    </h1>
    <p
      class="text-gray-500 dark:text-gray-400 font-heading font-semibold"
    >
      {data.subtitle}
    </p>
  </hgroup>
</header>
<main
  id="post-content"
  class="pt-8 pb-16 lg:pt-12 lg:pb-24 bg-white dark:bg-gray-900 font-content text-gray-900 dark:text-white antialiased"
>
  
  <div
    class="flex justify-between px-4 sm:px-6 lg:px-8 mx-auto max-w-screen"
  >
    <article
      class="mx-auto w-full max-w-4xl format format-sm sm:format-base lg:format-lg format-blue dark:format-invert"
      >
  {#await target}
    <p>loading...</p>
  {:then target}
    <svelte:component this={target} />
  {:catch error}
    <p>error: {error.message}</p>
  {/await}
    </article>
  </div>
</main>
