import type { SvelteComponent } from 'svelte';
import type { OrgMetadata } from 'ox-svelte';

export type Post = {
  original: string;
  slug: string;
  metadata: OrgMetadata;
};
