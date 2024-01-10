<script lang="ts">
	import type { PageData } from './$types';

	import { posts } from '$lib/data';

	export let data: PageData;
</script>

<svelte:component this={posts.find((post) => post.slug === data.slug)?.component} />
