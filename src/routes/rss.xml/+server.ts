import type { RequestHandler } from './$types';

import config from '$lib/config';
import * as post from '$lib/post';

export const prerender = true;

export const GET: RequestHandler = async ({}) => {
  const posts = await post.all();

  const headers = {
    'Content-Type': 'application/xml'
  };

  const xml = `
  <rss xmlns:atom="http://www.w3.org/2005/Atom" version="2.0">
			<channel>
        <title>${config.name}</title>
        <link>${config.url}</link>
        <description>${config.description}</description>
        <atom:link href="${config.url}/feed.xml" rel="self" type="application/rss+xml"/>
        ${posts.map(post => `
          <item>
            <title>${post.metadata.title}</title>
            <link>${config.url}/${post.slug}</link>
            <guid>${config.url}/${post.slug}</guid>
            <description>${post.metadata.subtitle}</description>
            <pubDate>${new Date(post.metadata.date).toUTCString()}</pubDate>
          </item>
        `).join('\n')}
			</channel>
		</rss>
	`.trim()

  return new Response(xml, { headers });
}
