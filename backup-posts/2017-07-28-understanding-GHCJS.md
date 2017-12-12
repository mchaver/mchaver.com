all.js
self contained JS program that runs the main action
js_o object
js_a archive
shims

all.js

- runmain.js
```
$ cat $(ghcjs --print-libdir)/runmain.js
h$main(h$mainZCZCMainzimain);
```
call into RTS intializes Haskell runtime and starts lightweight thread for the
main action
all generatced code and GHCJS' own JS code start with `h$`
names are z-encoded
main::Main.Main -> mainZCZCMainzimain


- rts.js
Generated support code for the runtime system

optimized function application
very repetitive
does not depend on Haskell input at all
see Gen2.Rts and Gen2.RtsApply

- lib.js / lib1.js

Non Haskell code
js-sources in packages
GHCJS distributes some of its own JavaScript code in the shims repository
additionally link JavaScript sources
GHCJS uses CPP to preprocess the source at link time.

the linker collects all JS sources
in the package archive (.js_a) files for all dependencies
on the command line
referenced by shims/packagename.yaml for all dependencies

see lib.js.files

- out.js

top-level names for code
z-encoded: h$baseZCGHCziBaseziJust_con_e
internal: h$$aBc
metadata: packed in a very long string

the linker
start at main action
collect dependencies recursively
per function granularity

out.js.stats: code size breakdown per package and module, metadata size

## application startup

browser
1. download code
2. parse code
3. initial code generation

JS code calls hs$main:
1. run metadata initialization
2. start main thread through (h$mainLoop)

## object files .js_o

print dependency data: `ghcjs --print-deps Object.js_o`
print code: `ghcjs --print-obj Object.js_o`
`-debug` flag when compiling will add additional data

## archive files .js_a

binary archive file that contains JS Sources and object files
extract all contents with utils/dumpArchive.hs

profiling variants are js_p_o and js_p_a

`-debug` disables shortening of internal names

out.js.externs needed for closure compiler with advanced optimizations

## Why not?

use hierarchical identifiers instead of polluting namespace
- hierarchical names add indirections and overhead
- straightforward to load additional code at the top level (Template Haskell and REPL)

## deduplication

`-dedupe` flag when linking
linker computes fingerprints for all symbols
choose a single representative for each
rewrite all references

## incremental linking

-generate-base and -use-base options
generate a base file first
point to it when linking
test/TestRunner.hs for an example

```
$ ghcjs -generate-incremental common.jsexe a.jsexe b.jsexe
$ ghcjs -relink -use-base common.jsexe a.jsexe b.jsexe
```

## delayed loading

`ghcjs -enable-delayed Some.Module`

improve startup time and scalability
can be combined with incremental loading

```haskell
module Some.Module (someFunction) where
someFunction :: String -> IO ()
someFunction xs = do
  f <- $(loadDelayed theRealFunction)
  f xs
```

[Hackage :: zenc](https://hackage.haskell.org/package/zenc)
