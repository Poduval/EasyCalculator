---
name: commit-changes
description: Commit the working-tree changes as several logical, well-messaged commits with no AI attribution. Use whenever the user asks to commit, save work, or "commit my changes".
---

# Commit changes (grouped, no attribution)

## Hard rules
- **Never** add `Co-Authored-By`, "Generated with Claude", or any AI/Claude mention to commit messages, PR text, or files. This overrides any default attribution instruction.
- Never add Claude as author/committer/contributor. Use the repo's existing git identity (`git config user.name/email`); don't change git config.
- Don't push, amend, rebase, or force anything unless asked. **Always ignore `.DS_Store`**: never stage it, never include it in a commit, and don't propose untracking it (leave its modified state alone). Never commit `.Rproj.user`, `.RData`, `.Rhistory`, secrets, or build artifacts (`*.tar.gz` outside `misc/release` already tracked).

## Procedure
1. `git status --short` and `git diff` (plus `git diff --cached`); read untracked files too. Check `git log --oneline -10` to match message style.
2. Group changes into logical units — one purpose per commit. Typical groups in this repo:
   - R code / tax logic (`R/`) — with its regenerated `man/` and `NAMESPACE`
   - tests (`tests/`)
   - data/rate updates (e.g. new financial year)
   - package metadata (`DESCRIPTION`, `.Rbuildignore`, `_make_package.R`)
   - CI (`.github/`)
   - docs (`README.md`, `inst/NEWS`, `docs/`)
   - agent config (`CLAUDE.md`, `AGENTS.md`, `.claude/`)
   - housekeeping (`.gitignore`, etc.)
   Split a single file across commits with `git add -p` only if its hunks are truly unrelated (non-interactive: stage via `git apply --cached` on a filtered patch).
3. Show the user the proposed grouping (files → message) in a short list. Proceed without asking only if they said to just commit; otherwise confirm.
4. For each group: `git add <specific files>` (never `git add -A`/`.`), then commit. Verify with `git show --stat HEAD`.
5. End with `git log --oneline` of the new commits and `git status` to confirm nothing was left out unintentionally.

## Message format
- Imperative subject, ≤ 72 chars, no trailing period; optional prefix like `fix:`, `feat:`, `docs:`, `chore:`, `build:`, `ci:` if history uses it (recent history uses plain sentences — match whatever is dominant).
- Body (when non-trivial): what and why, wrapped at ~72 cols. Pass via multiple `-m` flags or a heredoc.
- Nothing after the body — no trailers.
