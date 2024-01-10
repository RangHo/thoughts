import slugify from 'slugify';

import type { SvelteComponent } from 'svelte';

export interface Post {
  component: typeof SvelteComponent;
  slug: string;
  filename: string;
}

export const posts: Post[] = await Promise.all(Object.entries(import.meta.glob('$posts/*.svelte')).map(async ([filepath, loader]) => {
  const component = await loader().then((module) => {
    return (module as { default: typeof SvelteComponent }).default;
  });
  const filename = filepath
    .split('/')
    .pop()!
    .replace(/\.svelte$/, '')
    .replace(/_/g, ' ');
  const slug = slugify(filename, {
    replacement: '-',
    lower: true,
  });
  
  return {
    component,
    slug,
    filename
  };
}));
