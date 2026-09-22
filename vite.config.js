import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  // The Elm debugger is off: its inspector walks the whole model, and the
  // anagram dictionary's 196k-element arrays overflow the stack the moment one
  // reaches the model. Without it dev behaves like the production build.
  plugins: [elmPlugin({ debug: false })],
  server: {
    proxy: {
      "/guardian": {
        target: "https://www.theguardian.com",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/guardian/, ""),
      },
    },
  },
});
