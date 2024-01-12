/** @type {import('tailwindcss').Config} */
export default {
  content: [ 'src/**/*.{html,js,ts,svelte}' ],
  darkMode: 'class',
  theme: {
    extend: {},
    fontFamily: {
      'heading': ['"GyeonggiTitleM"', 'sans-serif'],
      'content': ['"GyeonggiBatang"', 'sans-serif'],
    },
  },
  plugins: [],
}

