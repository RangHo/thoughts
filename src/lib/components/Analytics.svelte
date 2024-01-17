<script lang="ts">
  import { browser } from "$app/environment";

  export let measurementId: string;

  if (browser) {
    (window as any).dataLayer = (window as any).dataLayer || [];
    (window as any).gtag = (...args: any[]) => (window as any).dataLayer.push(args);
    (window as any).gtag("js", new Date());
    (window as any).gtag("config", measurementId);
  }
</script>

<svelte:head>
  <script src="https://www.googletagmanager.com/gtag/js?id={measurementId}" async></script>
</svelte:head>
