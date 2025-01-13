/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.hs"],
  theme: {
    extend: {},
  },
  plugins: [require("@tailwindcss/typography"), require("daisyui")],
  daisyui: {
    themes: ["emerald"],
    darkTheme: "emerald",
  },
};
