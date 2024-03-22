/** @type {import('tailwindcss').Config} */
export default {
  content: ["src/**/*.{html,js,ts,svelte}"],
  darkMode: "class",
  theme: {
    extend: {},
    fontFamily: {
      heading: ['"DNFBitBitv2"', 'sans-serif'],
      content: ['"NeoDunggeunmo Pro"', 'sans-serif'],
      monospace: ['"NeoDunggeunmo Code"', 'monospace'],
    },
  },
  plugins: [require("@tailwindcss/typography")],
};
