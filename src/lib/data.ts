import slugify from 'slugify';

import type { SvelteComponent } from 'svelte';

export interface PostMetadata {
	title: string;
	subtitle?: string;
	author?: string;
	date: string;
	language?: string;
}

export interface Post {
	component: typeof SvelteComponent;
	slug: string;
	metadata: PostMetadata;
}

type PostComponent = {
	default: typeof SvelteComponent;
	metadata: PostMetadata;
};

export const posts: Post[] = await Promise.all(
	Object.entries(import.meta.glob('$posts/*.svelte')).map(async ([filepath, loader]) => {
		const module = await loader().then((module) => {
			return module as PostComponent;
		});
		const component = module.default;
		const metadata = module.metadata;
		const rawname = filepath
			.split('/')
			.pop()!
			.replace(/\.svelte$/, '')
			.replace(/_/g, ' ');
		const slug = slugify(rawname, {
			replacement: '-',
			lower: true
		});

		return {
			component,
			slug,
			metadata
		};
	})
);
