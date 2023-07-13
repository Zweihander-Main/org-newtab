import { createRequire } from 'module';
import { pathsToModuleNameMapper } from 'ts-jest';

const require = createRequire(import.meta.url);
const tsconfig = require('./tsconfig.json');

const commonProjectConfig = {
	testPathIgnorePatterns: ['/e2e/'],

	setupFiles: ['jest-webextension-mock'],

	extensionsToTreatAsEsm: ['.ts', '.tsx'],
	moduleNameMapper: pathsToModuleNameMapper(tsconfig.compilerOptions.paths, {
		prefix: '<rootDir>/',
	}),
	testEnvironment: 'jsdom',
	transform: {
		'^.+\\.ts?$': ['ts-jest', { isolatedModules: true, useESM: true }],
		'^.+\\.tsx?$': [
			'ts-jest',
			{ useESM: true, tsconfig: { jsx: 'react-jsx' } },
		],
	},
};

/**
 * @type {import('@jest/types').Config.InitialOptions}
 */
const config = {
	collectCoverage: true,
	coverageDirectory: 'coverage',
	coverageProvider: 'v8',

	watchPlugins: ['jest-watch-select-projects'],
	projects: [
		{
			...commonProjectConfig,
			displayName: 'unit',
			testEnvironment: 'jsdom',
			testPathIgnorePatterns: ['/e2e/'],
		},
		{
			...commonProjectConfig,
			displayName: 'eslint',
			runner: 'jest-runner-eslint',
			moduleFileExtensions: ['js', 'jsx', 'ts', 'tsx'],
			testPathIgnorePatterns: ['.*.d.ts$'],
			testMatch: ['<rootDir>/src/**/*'],
		},
		{
			...commonProjectConfig,
			displayName: 'prettier',
			runner: 'jest-runner-prettier',
			testMatch: ['<rootDir>/src/**/*', '<rootDir>/package.json'],
		},
		{
			...commonProjectConfig,
			displayName: 'stylelint',
			runner: 'jest-runner-stylelint',
			testMatch: ['<rootDir>/src/**/*'],
			moduleFileExtensions: ['css', 'scss'],
		},
	],
};

export default config;
