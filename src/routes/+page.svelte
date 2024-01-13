<script lang="ts">
  import { all } from '$lib/post';
</script>

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
