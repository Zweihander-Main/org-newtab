import {resolve} from 'path';
import { defineConfig, PluginOption, resolveConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { chromeExtension } from 'rollup-plugin-chrome-extension';
import manifest from './src/manifest.json';

export default defineConfig({
	root: './src',
	plugins: [react(), chromeExtension({ manifest })],
	build: {
		outDir: './../dist',
		emptyOutDir: true,
		rollupOptions: {
			input: {
				main: resolve(__dirname, 'src/index.html')
			}
		}
	},
});
