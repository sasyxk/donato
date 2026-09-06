---
name: github-issues
description: "Create and manage Donato GitHub issues for bugs and features, organize milestones, and link tickets to branches, pull requests and commits. Use for GitHub work tracking; use the commit skill when a Git commit is requested."
---

# GitHub issues and milestones

Use this skill for the repository's GitHub work tracking. An explicit request
to create or update a ticket authorizes that operation; prepare its concrete
content and proceed without requesting the same approval again. A request for
a draft stays local. Creating tickets does not authorize implementing features,
pushing commits, creating releases or changing account credentials.

## Repository context

- Repository: `sasyxk/donato`, default branch currently `main`. Verify the remote
  and current state before changing GitHub resources.
- GitHub CLI is available inside WSL distribution `Ubuntu`. From PowerShell in
  the repository root, run `wsl -d Ubuntu --exec gh ...`.
- Bugs and features are both issues. Use existing labels `bug` and `enhancement`
  respectively; inspect labels before adding new ones.
- The release milestone is **Road to 0.1.0**, initially milestone **1**. Associate
  tickets when their requested scope belongs to that release, rather than
  assigning every future issue to it automatically. A milestone is not a
  published GitHub release or a Projects board.
- [references/workflow.md](references/workflow.md) contains the command recipes,
  existing ticket links, authentication recovery and linking conventions.

## Prepare and publish the requested change

1. Check identity and access with the read-only commands in the reference. Git
   SSH access, Git author settings, the WSL CLI login and a connected GitHub app
   can differ. A successful push does not establish API authentication. Check
   an app's profile and repository permissions before using it for writes too.
2. List existing issues and milestones, including closed entries and all pages.
   Inspect plausible matches before creating a duplicate. For an update, fetch
   the specific issue and change only the requested fields; preserve unrelated
   labels, assignees, body content and state.
3. Write the body or JSON payload under ignored `build/github-work/`. Use a
   structured connector argument, `gh issue ... --body-file`, or
   `gh api ... --input` for multiline content. Avoid embedding Markdown in a
   shell command or passing quoted multiline Bash through PowerShell.
4. Perform the authorized write, then fetch the result. Verify title/body,
   labels, state and milestone as applicable, and report the actual URLs.
   Stop after an ambiguous failed write; inspect GitHub before retrying a create.

For bugs, describe the trigger, minimal supported Donato source, expected and
observed behavior, evidence for the cause, and concrete acceptance checks.
Distinguish executed reproductions from findings based on code inspection.
Summarize relevant findings rather than uploading local analysis reports.

For features, state the intended behavior and completion criteria. Separate
approved scope from design decisions still to make. `ProgramL.md` is a draft;
the implementation and `ProgramL-implemented.md` describe existing behavior.
For strings or other undefined semantics, include a design task rather than
presenting a proposed syntax or API as already decided. Follow the repository's
current issue language, presently English, unless the user requests otherwise.

## Maintain tracker checklists

The repository owner requests automatic maintenance of the relevant tracker
entry during related work. Add one unchecked item for a newly reported bug or
task, preserving its identifier (for example, F5). Update an existing item in
place rather than duplicating it, and check it only after implementation and
validation are complete. Analysis alone leaves the item unchecked.

After a related commit is created, record a short fix summary, the validation
result and a Markdown link to `https://github.com/<owner>/<repo>/commit/<full-sha>`
in that entry. If a later corrective commit addresses the same item, replace
its fix link with the latest relevant commit and update the summary as needed.
Do not substitute an unrelated newer commit. If the commit is still local,
mark the link as pending the owner's push; on later related work, check its
publication status and remove that note once it is available on GitHub.
Updating the tracker does not authorize pushing. Preserve other checklist
entries and keep the tracker open when completing an individual item.

## Authentication and private information

Never put passwords, access tokens, authorization headers, private keys,
credential-file contents, or unnecessary personal paths/emails in skills,
scripts, issue bodies or logs. Let `gh` use its own credential storage; do not
retrieve or print its token. Use repository-relative paths in public examples.

On HTTP 401, preserve completed drafts and request CLI reauthentication through
the browser. On 403/404, check the account, repository and permissions before
assuming the resource is absent. Repeatedly trying the same invalid credentials
does not help. Do not rotate, upload or delete SSH keys as an API-auth repair.
If user interaction is required, first finish the reviewable drafts, then
explain the specific access problem and the next command.

## Scripts and later development

Use direct CLI commands for a small operation. For typed JSON payloads or a
batch, [scripts/github_api.py](scripts/github_api.py) provides paginated reads
and authenticated writes from UTF-8 files. Writes default to a local preview;
pass `--apply` once the operation is authorized. See the reference for examples.
The helper does not infer desired changes, deduplicate creates or retry writes;
those decisions remain with the caller. Keep batch inputs and results in `build/`.

Start commit subjects with `#N` for related work that leaves the issue open,
including individual entries in a tracker. Use `Fixes #N` for a fully resolved
bug issue and `Closes #N` for a completed feature issue, followed by a short
description, as required by the repository's `commit` skill. For a closing PR,
also put the closing reference in its description so GitHub links the PR.
Use the issue's Development controls for an explicitly linked branch; putting
a number in the branch name alone is only a convention.
If a commit is also requested, apply the repository's `commit` skill.
