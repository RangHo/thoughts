import slugify from 'slugify';

import type { SvelteComponent } from 'svelte';

export interface PostMetadata {
	title: string;
	subtitle?: string;
	author?: string;
	date: string;
	language: string;
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

const imports = import.meta.glob('$posts/*.svelte');

export async function all() {
  let posts: Post[] = [];

  for (const filepath in imports) {
    const loader = imports[filepath];
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
    posts.push({
      component,
      slug,
      metadata
    });
  }

  return posts;
}

export async function getMetadata(slug: string): Promise<PostMetadata> {
  const posts = await all();
  
  const post = posts.find((post) => post.slug === slug);
  
  if (post) {
    return post.metadata;
  } else {
    throw new Error(`Post not found: ${slug}`);
  }
}

export async function getComponent(slug: string): Promise<typeof SvelteComponent> {
  const posts = await all();

  const post = posts.find((post) => post.slug === slug);

  if (post) {
    return post.component;
  } else {
    throw new Error(`Post not found: ${slug}`);
  }
}
