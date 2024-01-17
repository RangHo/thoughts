<script lang="ts">
  import Analytics from "$lib/components/Analytics.svelte";

  import "../app.postcss";

  import { browser } from "$app/environment";

  import { colorMode } from "$lib/dark";

  colorMode.subscribe((value) => {
    if (browser) {
      document.documentElement.classList.remove("dark", "light");
      document.documentElement.classList.add(value);
    }
  });
</script>

<Analytics measurementId="G-E86FB6SVBE" />

<button on:click={() => colorMode.update((v) => (v === "dark" ? "light" : "dark"))}>
  Toggle
</button>

<div
  class="transition font-content bg-gray-100 dark:bg-gray-900 text-gray-900 dark:text-gray-100 antialiased"
>
  <slot />
</div>
