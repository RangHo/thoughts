<script lang="ts">
  import '../app.postcss';

  import { browser } from '$app/environment';

  import { colorMode } from '$lib/dark';

  colorMode.subscribe((value) => {
    if (browser) {
      document.documentElement.classList.remove('dark', 'light');
      document.documentElement.classList.add(value);
    }
  });
</script>

<button on:click={() => (colorMode.update((v) => (v === 'dark' ? 'light' : 'dark')))}>
  Toggle
</button>

<slot />
