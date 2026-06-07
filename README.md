# mchaver.com

```
cabal update
cabal build
cabal run site -- build
cabal run site -- watch
cabal run site -- clean
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
