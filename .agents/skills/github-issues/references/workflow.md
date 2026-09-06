# GitHub work-tracking recipes

## Access from PowerShell

Run these commands from the repository root:

```powershell
git remote -v
wsl -d Ubuntu --exec gh api user --jq .login
wsl -d Ubuntu --exec gh api repos/sasyxk/donato --jq '{full_name,default_branch,has_issues,permissions}'
```

The normal publishing identity for this repository is `sasyxk`. Verify current
access rather than copying cached credentials. For API authentication recovery:

```powershell
wsl -d Ubuntu --exec gh auth login --hostname github.com --git-protocol ssh --web
```

The user completes the browser flow as the intended account. If prompted to
upload an SSH key, choose Skip when keeping the existing working SSH setup.
Check `gh auth login --help` before suggesting optional flags: the CLI used
here did not support `--skip-ssh-key`. Recheck `gh api user --jq .login` after
login. If an SSH key was uploaded, do not assume previous keys were removed.

Only when investigating SSH separately:

```powershell
ssh -T -o BatchMode=yes -o StrictHostKeyChecking=yes -o ConnectTimeout=10 git@github.com
```

GitHub's successful greeting identifies the account. That SSH probe normally
exits with status 1 because GitHub does not provide shell access. It does not
test the API token or prove repository-specific write permission.

## Inspect existing work

```powershell
wsl -d Ubuntu --exec gh api --paginate 'repos/sasyxk/donato/issues?state=all&per_page=100' --jq '.[] | {number,title,state,milestone:.milestone.title}'
wsl -d Ubuntu --exec gh api --paginate 'repos/sasyxk/donato/milestones?state=all&per_page=100' --jq '.[] | {number,title,state,html_url}'
wsl -d Ubuntu --exec gh api --paginate 'repos/sasyxk/donato/labels?per_page=100' --jq '.[].name'
wsl -d Ubuntu --exec gh api repos/sasyxk/donato/issues/3
```

The issues endpoint also returns pull requests; exclude entries with a
`pull_request` property when matching issue candidates. Do not rely only on
search indexing immediately after a write. `gh api --paginate` can print
multiple JSON documents; use page-by-page Python reads or the helper below
when a script needs one JSON array.

Existing anchors (fetch current state; these are not a snapshot of completion):

| Resource | URL |
| --- | --- |
| Road to 0.1.0 | https://github.com/sasyxk/donato/milestone/1 |
| Integer condition conversion | https://github.com/sasyxk/donato/issues/1 |
| Double arithmetic and equality | https://github.com/sasyxk/donato/issues/2 |
| Pattern matching | https://github.com/sasyxk/donato/issues/3 |
| Native Windows 11 | https://github.com/sasyxk/donato/issues/4 |
| String definition, printing and manipulation | https://github.com/sasyxk/donato/issues/5 |
| Minor release bugs tracker | https://github.com/sasyxk/donato/issues/6 |
| Project agent skills tracker | https://github.com/sasyxk/donato/issues/7 |

## Create or edit an issue

After reviewing matching issues, save the exact Markdown to
`build/github-work/issue.md`. For example, from PowerShell:

```powershell
New-Item -ItemType Directory -Force build/github-work | Out-Null
$body = @'
## Goal

Describe the requested behavior and the current limitation.

## Acceptance criteria

- [ ] State observable completion criteria for this specific task.
'@
$utf8 = New-Object System.Text.UTF8Encoding($false)
[IO.File]::WriteAllText((Join-Path $PWD 'build/github-work/issue.md'), $body, $utf8)
```

Adapt the title, body, label and milestone to the actual request. These are
write-command recipes, not instructions to open an example ticket:

```powershell
wsl -d Ubuntu --exec gh issue create --repo sasyxk/donato --title 'Describe the requested feature' --label enhancement --milestone 'Road to 0.1.0' --body-file build/github-work/issue.md
wsl -d Ubuntu --exec gh issue edit 3 --repo sasyxk/donato --milestone 'Road to 0.1.0'
wsl -d Ubuntu --exec gh issue edit 3 --repo sasyxk/donato --add-label enhancement
```

Use `bug` for a bug report. Omit `--milestone` when the issue is not assigned
to a release. `--body-file` on edit replaces the whole body: fetch and preserve
the existing text when only a portion should change. Use `--add-label` for
additive edits; replacing the REST `labels` array replaces the complete set.

## Create or update a milestone

For a new, requested milestone, save a UTF-8 JSON payload such as:

```json
{
  "title": "Road to 0.2.0",
  "description": "Describe the agreed release scope.",
  "state": "open"
}
```

This is an illustrative future version, not an instruction to create it now.
First check for a matching milestone. Then use the appropriate operation:

```powershell
wsl -d Ubuntu --exec gh api repos/sasyxk/donato/milestones --method POST --input build/github-work/milestone.json
wsl -d Ubuntu --exec gh api repos/sasyxk/donato/milestones/1 --method PATCH --input build/github-work/milestone-edit.json
```

On edit, send only the fields requested. Set no due date unless one is known
from the user's instructions. REST issue payloads use the milestone **number**,
for example `{"milestone": 1}`; `gh issue edit --milestone` uses its **title**.
Associating an issue with another milestone moves it from its previous one.

## Reusable helper and batch scripts

The helper uses the installed `gh`, not a token argument. These commands run
from PowerShell at the repository root:

```powershell
# Fetch all issues as one JSON array (read-only).
wsl -d Ubuntu --exec python3 .agents/skills/github-issues/scripts/github_api.py 'issues?state=all' --paginate

# Preview a prepared milestone payload locally; no network request.
wsl -d Ubuntu --exec python3 .agents/skills/github-issues/scripts/github_api.py milestones --method POST --input build/github-work/milestone.json

# Perform that operation only when requested and reviewed.
wsl -d Ubuntu --exec python3 .agents/skills/github-issues/scripts/github_api.py milestones --method POST --input build/github-work/milestone.json --apply
```

`--repo` and `--expected-login` default to the current repository/account and
can be supplied explicitly for another authorized context. `--apply` verifies
identity and repository write permission before sending a write. The helper
uses GitHub.com endpoints relative to the selected repository, accepts GET,
POST and PATCH, and prints JSON results. It does not manage credentials.

For several issues, create a short Python script under `build/github-work/`:

1. Load each reviewed Markdown body with `Path.read_text(encoding="utf-8")` and
   serialize the required fields with `json.dumps`, keeping multiline text intact.
2. List issues and milestones first. Inspect matching titles; reuse a suitable
   milestone and edit the intended issue by number when appropriate.
3. Call the helper with `subprocess.run([...], check=True, capture_output=True,
   text=True)` and an argument list, without `shell=True`. Alternatively use
   `gh issue create --body-file` directly after the same identity checks.
4. Process writes sequentially. Record each returned issue number and URL in
   `build/github-work/results.json` immediately so a later failure is recoverable.
5. Fetch each changed resource and verify the requested fields. If a create
   times out, inspect recent resources before attempting another create.

Do not blindly replay the old five-issue roadmap publisher. Its inputs described
one completed task; future batches must contain only the new requested changes.

## Link commits, PRs and branches

- Start the commit subject with `#N <description>` for related work that leaves
  the issue open. Use this form for individual fixes or additions in a tracker.
- Use `Fixes #N <description>` for a fully resolved bug issue and
  `Closes #N <description>` for a completed feature issue. These close the issue
  once the commit reaches the default branch; all issue criteria must be met.
- Also put a closing reference in the PR **description** when the PR targets
  `main`; GitHub links that PR and closes the issue on merge. A closing keyword
  only in a commit message does not establish the same PR sidebar link.
- In the issue sidebar, **Development > Create a branch** creates a linked
  branch. A name such as `feature/3-pattern-matching` alone does not link it.
- For another repository, use the full reference, e.g. `sasyxk/donato#3`.

Official references: [GitHub CLI](https://cli.github.com/manual/),
[milestones](https://docs.github.com/en/issues/using-labels-and-milestones-to-track-work/about-milestones),
[linking PRs and issues](https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue),
[linked branches](https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-a-branch-for-an-issue).
