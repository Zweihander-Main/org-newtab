import esbuild from 'esbuild';

esbuild
	.build({
		entryPoints: ['src/App.tsx'],
		bundle: true,
		outfile: 'dist/out.js',
	}).catch(() => process.exit(1))
