<script lang="ts">
  import { onMount } from 'svelte';
  import { all, type Post } from '$lib/post';

  let posts: Post[] = [];

  onMount(async () => {
    posts = await all();
  });
</script>

<h1>take a look at my thoughts</h1>

<ul>
  {#each posts as post}
    <li>
      <a href="/{post.slug}">
        {post.metadata.title}
      </a>
    </li>
  {/each}
</ul>
