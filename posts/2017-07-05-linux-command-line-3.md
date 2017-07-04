---
title: Linux Command Line - find, which, whereis, grep, awk, sed
---

A short reference for common search tools.

## find

`find` is an executable for finding a file based on its name.

`find .` return the files and directories in the current directory.

`find . -name <pattern>` return the files and directories of the current 
directory that match a pattern such as `\*.txt` or `home\*`.

There is an alternative program `locate` that is faster, but it requires a 
database that must be up to date with `updatedb` or it can return incorrect 
information. `locate` is also less standardized across systems.

## which

`which` returns the absolute path of an executable.

```bash
~$ which sudo
/user/bin/sudo
```

## whereis

`whereis` returns the locations of the binary, source and man pages of an 
executable.

## grep

`grep` stands for Global Regular Expression Print. It is useful for searching 
the contents of a file or multiple files. It can also be used in a Unix pipe 
for filtering data. GNU grep is faster than BSD grep, but it is not packaged 
with Mac OS X. You can install `ggrep` with 
`brew tap homebrew/dupes; brew install grep`.

`grep "string" <file>` returns each line of the file that contains `string`.

`grep "string" <pattern>` returns each line of any file that contains `string`,
prefixed by the name of a file.

The `-i "string"` flag makes matching `string` case insensitive.

`grep "regex" <pattern>` returns file lines based on matches for a regular 
expression.

`grep -iw "string" <pattern>` returns matches to `string` that is a full world.
It matches `This is a string.`, but not `This is a shoestring.`.

`... | grep "regex"` filters the results of another command.

`grep -r "string" *` searches recursively in the current directory.

## awk

`awk` manipulates structured data and returns reports. It has the following 
features

- text file as records and fields
- variables, conditions and loops
- arithmetic and string operators
- generate formatted reports

`awk '{print;}' <file>` prints every line of a file.

```bash
$ awk '/one/
> /two/' report.txt
```

Finds `one` or `two`. The newline acts as or.

`awk '{print $1,$2}' test.txt` prints the first two columns. `$1` is the first 
item in a column.

`$NF` is the last column.

## sed

`sed` is a stream editor. It operates on one line of a file at a time and makes 
changes for each line.

`sed [options] commands <file>`

`sed '' test.txt` returns every line of the file.

`sed '1p' test.txt` prints the first line. `sed '1,5p' test.txt` prints the 
first five lines.

`'1,+4p'` matches the first line plus four.

`'1~2p'` is a range.

`'1~2d'` deletes the first two lines.

`-n` suppresses automatic printing.

`-i` applies the filter directly to the file.

`-i.bak` applies the filter directly to the file and creates a backup with `.bak`
at the end.

`'s/old/new'` replaces the first instance of `old` with `new`.

`'s/old/new/g'` replaces all instances of `old` with `new`.

`'s/old/new/2'` replaces the first two instances of `old` with `new`.
