import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm'

// https://vitejs.dev/config
export default defineConfig({
  plugins: [elmPlugin()]
})
