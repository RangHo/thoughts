import type { PageServerLoad } from "./$types";

import { error } from "@sveltejs/kit";

import * as post from "$lib/post";

export const load: PageServerLoad = async ({ params }) => {
  const { slug } = params;

  try {
    const target = await post.getMetadata(slug);

    return {
      slug,
      title: target.title,
      subtitle: target.subtitle || "",
      author: target.author || "",
      date: target.date,
      language: target.language,
    };
  } catch (err) {
    throw error(404, "Post not found");
  }
};

export const entries = async () => {
  return post.all();
};
