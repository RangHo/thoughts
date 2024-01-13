<script lang="ts">
  import Giscus from '@giscus/svelte';
  import { ScaleOut } from 'svelte-loading-spinners';

  import { getComponent } from '$lib/post';
  import { colorMode } from '$lib/dark';

  import type { PageData } from './$types';

  import './styles.postcss';

  export let data: PageData;

  let target = getComponent(data.slug);
</script>

<header
  class="pt-8 font-content antialiased"
>
  <hgroup
    class="pt-2 pb-4 mx-auto max-w-4xl border-b-2 border-gray-200 dark:border-gray-700 text-center"
  >
    <h1 class="pb-2 text-3xl lg:text-4xl font-heading font-bold">
      {data.title}
    </h1>
    <div class="flex flex-col justify-center">
      <p class="text-xl font-heading font-semibold text-gray-400 dark:text-gray-500">
        {data.subtitle}
      </p>
      <p class="mt-2 text-sm font-heading font-semibold text-gray-400 dark:text-gray-500">
        {data.date}
      </p>
    </div>
  </hgroup>
</header>
<main
  class="pt-8 pb-16 lg:pt-12 lg:pb-24"
>
  <div class="flex flex-col justify-between px-4 sm:px-6 lg:px-8 mx-auto max-w-screen">
    <article class="mx-auto w-full max-w-4xl prose dark:prose-invert">
      {#await target}
        <div class="flex justify-center">
          <ScaleOut color="black" />
        </div>
      {:then target}
        <svelte:component this={target} />
      {:catch error}
        <p>error: {error.message}</p>
      {/await}
    </article>

    <div class="mx-auto w-full max-w-4xl">
      <Giscus
        repo="RangHo/thoughts.rangho.me"
        repoId="R_kgDOHtWVvw"
        category="Comments"
        categoryId="DIC_kwDOHtWVv84CT8zK"
        mapping="pathname"
        strict="0"
        reactionsEnabled="1"
        emitMetadata="0"
        inputPosition="top"
        theme={$colorMode}
        lang={data.language}
      />
    </div>
  </div>
</main>
