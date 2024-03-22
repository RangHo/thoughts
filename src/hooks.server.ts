import type { Handle } from '@sveltejs/kit';

// import { getMetadata } from "$lib/post";

// export const handle: Handle = async ({ event, resolve }) => {
//   const slug = event.params.slug || "";

//   let language: string;
//   if (slug === "") {
//     language = "ko";
//   } else {
//     language = (await getMetadata(slug)).language;
//   }

//   return await resolve(event, {
//     transformPageChunk({ html }) {
//       return html.replace("%lang%", language);
//     },
//   });
// };
