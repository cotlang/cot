# Git Hooks

This directory contains git hooks for the Cot project.

## Setup

To enable these hooks, run:

```bash
git config core.hooksPath .githooks
```

## Available Hooks

### pre-commit

Runs before each commit:
1. Checks code formatting (`zig fmt --check src/`)
2. Builds the project (`zig build`)
3. Runs all tests (`zig build test`)

If any check fails, the commit is aborted.

## Bypassing Hooks

To commit without running hooks (not recommended):

```bash
git commit --no-verify -m "message"
```
