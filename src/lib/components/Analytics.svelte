<script lang="ts">
  import { browser } from '$app/environment';

  export let measurementId;

  if (browser) {
    (window as any).dataLayer = (window as any).dataLayer || [];
    (window as any).gtag = (...args: any[]) => (window as any).dataLayer.push(args);
    (window as any).gtag('js', new Date());
    (window as any).gtag('config', measurementId);
  }
</script>

<svelte:head>
  <script src="https://www.googletagmanager.com/gtag/js?id=G-E86FB6SVBE" async></script>
</svelte:head>
