import type { SvelteComponent } from 'svelte';

export type PostMetadata = Record<string, string>;

export type PostModule = {
  default: typeof SvelteComponent<any>;
  metadata: PostMetadata;
};

export type Post = {
  original: string;
  slug: string;
  metadata: PostMetadata;
};
