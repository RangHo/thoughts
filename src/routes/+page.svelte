<script lang="ts">
	import { posts } from '$lib/data';
</script>

{#each posts as post}
	<a href={post.slug}>{post.slug}</a>
{/each}
