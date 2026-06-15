# mchaver.com

```
cabal update
cabal build
cabal run site -- build
cabal run site -- watch
cabal run site -- clean   # avoid: deletes _site (breaks the deploy worktree) — see Deploying
```

## Literate Haskell posts

Some posts under `posts/` are literate Haskell (`.lhs`). Literate Haskell combines prose and code for text files that are instructive and compilable at the same time. You can type check and run an `.lhs` file by building a throwaway cabal project.

```
mkdir /tmp/lhs-check && cd /tmp/lhs-check
cp /path/to/posts/2026-06-03-babys-first-effects-with-haskell-effectful.lhs Main.lhs
cat > check.cabal <<'EOF'
cabal-version: 2.4
name: check
version: 0
executable check
  main-is: Main.lhs
  build-depends: base, effectful, effectful-core, containers
  default-language: Haskell2010
EOF
cabal run        # compiles and runs main
```

## Deploying

The site is a static build served by nginx. The built output lives on an orphan 
`deploy` branch, kept separate from the source on `master`. `_site/` is a git 
worktree checked out to that branch.
`cabal run site -- build` writes straight into it.

```
master  → source (site.hs, posts/, templates/…)   ← edit here
deploy  → _site output only                        ← the server tracks this
```

To publish, run the deploy script, then pull on the server:

```
./deploy.sh                                    # build + push the deploy branch
ssh server 'cd /data/www-mchaver && git pull'  # go live
```

`deploy.sh` clears `_cache`/`_store` and `_site`'s contents (a full rebuild, so
template edits don't leave stale HTML), rebuilds, then commits and pushes the
output to `deploy`.

> **Do not run `cabal run site -- clean` in this checkout.** It deletes the
> `_site` directory, which destroys the worktree pointer (`_site/.git`). Use
> `./deploy.sh` instead. If the worktree ever breaks, recreate it with:
> ```
> git worktree prune && rm -rf _site && git worktree add _site deploy
> ```

### Server one-time setup

Clone the `deploy` branch directly into the nginx document root:

```
sudo git clone --branch deploy --single-branch \
  git@github.com:mchaver/mchaver.com.git /data/www-mchaver
```
