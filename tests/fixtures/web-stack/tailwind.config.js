// Tailwind CSS configuration anchor — picked up by lsp-tailwindcss
// when traversing project roots, enables Tailwind activation in any
// HTML / TSX / Hiccup buffer rooted here.
module.exports = {
  content: ['./**/*.{html,tsx,clj}'],
  theme: { extend: {} },
  plugins: [],
};
