import { fileURLToPath } from 'node:url'
import globals from 'globals'
import { includeIgnoreFile } from '@eslint/compat'

import css from '@eslint/css'
import js from '@eslint/js'
import svelte from 'eslint-plugin-svelte'
import ts from 'typescript-eslint'

import stylistic from '@stylistic/eslint-plugin'
import unocss from '@unocss/eslint-plugin'

const gitignorePath = fileURLToPath(new URL('./.gitignore', import.meta.url))

export default ts.config(
  // Re-use the .gitignore file.
  includeIgnoreFile(gitignorePath),
  // Per-language configurations.
  css.configs.recommended,
  js.configs.recommended,
  ts.configs.recommended,
  svelte.configs['flat/recommended'],
  {
    files: ['**/*.svelte', '**/*.svelte.ts'],
    languageOptions: {
      parserOptions: {
        parser: ts.parser,
      },
    },
  },
  // Runtime environment.
  {
    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.node,
      },
    },
  },
  // Style-related.
  stylistic.configs.recommended,
  // External utilities.
  unocss.configs.flat,
)
