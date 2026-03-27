import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
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
