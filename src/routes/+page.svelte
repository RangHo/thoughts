<script lang="ts">
  import { MetaTags } from 'svelte-meta-tags';
  import { all } from '$lib/post';
  import config from '$lib/config';
</script>

<MetaTags
  title={config.name}
  canonical={config.url}
  openGraph={{
              url: config.url,
              title: config.name,
              siteName: config.name,
            }}
  twitter={{
            handle: '@RangHo_777',
            cardType: 'summary',
            title: config.name,
          }}
/>

<h1>take a look at my thoughts</h1>

{#await all()}
  <p>loading...</p>
{:then posts}
  <ul>
    {#each posts as post}
      <li>
        <a href="/{post.slug}">
          {post.metadata.title}
        </a>
      </li>
    {/each}
  </ul>
{:catch error}
  <p>error: {error.message}</p>
{/await}
