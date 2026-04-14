import { defineConfig } from "vite";
import { resolve } from "node:path";

export default defineConfig({
  base: "/causeway/",
  build: {
    outDir: resolve(__dirname, "../../../website/static/causeway"),
    emptyOutDir: true,
    target: "es2022",
    rollupOptions: {
      input: {
        main: resolve(__dirname, "index.html"),
        "hw-stub": resolve(__dirname, "hw-stub.html"),
      },
    },
  },
  resolve: {
    alias: { "@": resolve(__dirname, "src") },
  },
  test: {
    include: ["tests/**/*.spec.ts"],
    environment: "node",
  },
});
