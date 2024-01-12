import type { PageServerLoad } from './$types';

import { error } from '@sveltejs/kit';

import * as post from '$lib/post';

export const load: PageServerLoad = async ({ params }) => {
	const { slug } = params;

  try {
    const target = await post.getMetadata(slug);

	  return {
		  slug: target.slug,
      title: target.metadata.title,
      subtitle: target.metadata.subtitle || '',
      author: target.metadata.author || '',
      date: target.metadata.date,
      language: target.metadata.language,
	  };
  } catch (err) {
    throw error(404, 'Post not found');
  }
};

export const entries = async () => {
  return post.all();
};
