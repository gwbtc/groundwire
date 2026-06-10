import { defineConfig } from "vite";
import { resolve } from "node:path";
import { nodePolyfills } from "vite-plugin-node-polyfills";

export default defineConfig({
  base: "/causeway/",
  plugins: [
    // @ngraveio/bc-ur transitively pulls Node's `util`/`assert` shims which
    // need `process` and `Buffer` as globals. Polyfill them.
    nodePolyfills({
      include: ["buffer", "process", "util", "assert"],
      globals: { Buffer: true, global: true, process: true },
    }),
  ],
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
  server: {
    // Localhost is a secure context — camera APIs work over plain HTTP here.
    // Phones don't need access; they just optically scan QRs off the screen.
    proxy: {
      "/_proxy/urbwatcher": {
        target: "http://143.198.70.9:8081",
        changeOrigin: true,
        rewrite: (p) => p.replace(/^\/_proxy\/urbwatcher/, ""),
      },
      "/_proxy/mempool": {
        target: "https://mempool.space",
        changeOrigin: true,
        rewrite: (p) => p.replace(/^\/_proxy\/mempool/, ""),
      },
      "/_proxy/sponsor": {
        target: "http://143.198.70.9:8081",
        changeOrigin: true,
        rewrite: (p) => p.replace(/^\/_proxy\/sponsor/, ""),
      },
    },
  },
  test: {
    include: ["tests/**/*.spec.ts"],
    environment: "node",
  },
});
