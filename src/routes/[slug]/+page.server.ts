import type { PageServerLoad } from './$types';

import { error } from '@sveltejs/kit';

import { posts } from '$lib/data';

export const load: PageServerLoad = async ({ params }) => {
	const { slug } = params;

	const post = posts.find((post) => post.slug === slug);

	if (!post) {
		throw error(404, 'Post not found');
	}

	return {
		slug: post.slug,
		metadata: post.metadata
	};
};
