#!/usr/bin/env bash
# Build the site and publish it to the `deploy` branch.
#
# `_site/` is a git worktree checked out to the orphan `deploy` branch, which
# holds ONLY the built output. `master` stays source-only. After this script
# pushes, deploy the server with:
#
#     cd /data/www-mchaver && git pull
#
set -euo pipefail
cd "$(dirname "$0")"

# Fresh build. Do NOT use `cabal run site -- clean`: it deletes the _site
# directory, which destroys the git worktree pointer (_site/.git). Instead clear
# the Hakyll caches (fixes stale HTML after template edits) and wipe _site's
# contents while preserving the worktree link.
rm -rf _cache _store
find _site -mindepth 1 -maxdepth 1 ! -name '.git' -exec rm -rf {} +
cabal run site -- build

# Commit + push the built output from the _site worktree (branch: deploy).
cd _site
git add -A
if git diff --cached --quiet; then
  echo "No changes to deploy."
  exit 0
fi
git commit -m "Site build: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
git push origin deploy

echo
echo "Pushed to 'deploy'. On the server:  cd /data/www-mchaver && git pull"
