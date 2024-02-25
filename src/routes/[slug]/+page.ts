import type { PageLoad } from "./$types";

import { error } from "@sveltejs/kit";

import { posts } from "$lib/post";

export const load: PageLoad = async ({ params }) => {
  const { slug } = params;
  const target = posts.find((post) => post.slug === slug);

  try {
    const post = await import(`../../posts/${target?.original}.svelte`);

    return {
      component: post.default,
      ...target
    };
  } catch (err) {
    error(404, "Post not found");
  }
};
