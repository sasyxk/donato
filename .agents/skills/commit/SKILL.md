---
name: commit
description: "Review and commit changes in the Donato repository when a Git commit is requested. Write clear commit messages, select relevant validation, and keep local reports and build artifacts out of Git."
---

# Commit

Create a local commit covering the user's authorized scope. An existing request
to commit is sufficient authorization; do not ask for another confirmation.
Treat pushing or rewriting existing commits as separate actions requiring their
own authorization.

## Review the changes

- Read the repository's `AGENTS.md` and inspect the branch, working tree, staged
  diff and recent commit subjects. Include untracked files in the review.
- If the user requests all current changes, include the reviewed, non-ignored
  changes together. For a narrower request, stage only its files or hunks and
  preserve unrelated work.
- Keep `report/` and `build/` local. Do not force-add them or copy report contents
  into a commit message. Check the staged paths as well as `.gitignore`: ignore
  rules do not remove files that were already tracked.
- Preserve `ProgramL.md` unless the user explicitly requests changes to that
  original draft. Reusable sources and expected output under `examples/` and
  repository skills under `.agents/skills/` belong in Git.

## Validate the commit's scope

- For compiler or build changes, use the README's Ubuntu/WSL commands and selected
  Clang/LLVM versions. Compile Donato programs sequentially from `build/`, because
  the compiler shares `output.ll` and `output.o` between invocations.
- Check diagnostics, creation of a fresh executable, execution status and output;
  `dtc` can return zero after external tool failures. The existing example checker
  implements these checks at the chosen optimization level.
- Reuse completed validation when it still covers the unchanged implementation.
  Do not rebuild for changes limited to prose, checklist formatting or this skill.
- Review the final staged diff and check whitespace. Preserve intentional source
  contents and existing line endings rather than applying unrelated formatting.

## Write and create the commit

- Use an informative, imperative subject in the repository's message language
  (currently English), unless the user requests another language or convention.
- Scale the body to the change. For substantial commits, explain the concrete
  configuration or behavior changes, supporting documentation/examples, and
  relevant validation. Include versions or commands only when they aid review.
- Describe what is actually included. In particular, distinguish a C++ toolchain
  update from an LLVM backend migration, and documentation or analysis from an
  implemented compiler fix. Avoid conversational history and claims unsupported
  by the diff or observed results.
- Save a multiline message as UTF-8 in an ignored file such as
  `build/commit-message.txt`, then use `git commit -F` with that file. Preserve
  actual newlines and literal command examples when invoking Git from PowerShell.
- After committing, verify the commit's file list and working-tree status. Report
  its hash, subject, substantive changes and validation; identify any remaining
  work without implying a push has occurred.
