---
title: TeX Core Concepts
tags: TeX
---

TeX is a program that typesets boxes. TeX builds pages based on boxes and glue. 
Each character is a box which is glued to other letters to form words. The glue 
is elastic so it can be manipulated to fill the page optimally. Boxes may be 
contained by other boxes.

## Control character

A control character (non-printing character) is a code point (a number) in a 
character set, that does not represent a written symbol.

## Control sequence

A `\` followed by a sequence of control characters.

- **TeX primitives** are native commands: `\def`, `\font`, etc.
- **macros** are sequences defined with `\def`.
- **registers** are variables to save values: `\baselineskip`, `\countdef`, 
`\dimendef`.
- **character constants** are sequences declared by the primitives `\chardef` or 
`\mathchardef`: `\$`, `\alpha`, `\sum`.
- **font selectors** are sequences declared by the primitive `\font`.

An undefined control sequence is any control sequence which has no meaning 
assigned to it.

## Boxes

TeX has for primitive box control sequences.

- `\hbox` horizontal box.
- `\vbox` vertical box with reference point at the last item inside it.
- `\vtop` vertical box with reference point at the first item inside it.
- `\vcenter` vertical box with reference point in the middle.

#### baseline

The baseline of a box is a vertical line that divides the box. It is used as 
a reference point for placing things in the box. Characters like `'x'` start at 
or slightly above the baseline and never cross it. Other characters like `'y'`
rest on the baseline and its tail crosses it.

#### height

The distance from the baseline to the top of the box.

#### depth

The distance from the baseline to the bottom of the box.

#### width

The horizontal length of a box.

#### reference point

Where the baseline and the left-side of the box meet.

#### badness

Badness is a measure for the quality of a box made by TeX. It is an integer value
between 0 and 10000. `\badness` is set by TeX each time it constructs a box. If 
there is an overflow error then `\badness` is set to 1000000.

#### overfull error

TeX could not produce a result that does not overfill a box. This is generally 
do to a justification or hyphenation error.

#### underfull error

TeX could not appropriately fill the box. TeX could not stretch the box wide 
enough without making the space bigger than permitted.

#### characters
In TeX, characters are stored in boxes like every printed element. 

## Glue

Glue is the white space between boxes. Glue can expand and contract during the 
construction to give optimal positioning for boxes. At some point becomes set 
and cannot change. It is displayed as white space.

## Modes

When processing a `tex` source file, `TeX` operates in one of the following 
modes.

#### vertical mode

TeX stack boxes on top of each other in vertical lists.

#### internal vertical mode

TeX builds a vertical list for a vbox.

#### horizontal mode

TeX stacks boxes next to each other in horizontal lists.

#### restricted horizontal mode

TeX builds a horizontal list for a hbox. No line break allowed.

#### math mode

TeX builds a mathematical formula for a horizontal list.

#### display math mode

TeX builds a mathematical formula to be placed on a line itself. This interrupts 
the current paragraph.

## Macros

A macro is a rule that specifies how a control sequence should be mapped to a 
replacement output sequence. The primitive control sequence `\def` allows us
to define new macros. Macros support up to nine arguments.

```latex
\def <command> <parameter-text>{<replacement-text>}
```

The first argument must be delimited by two square brackets while the second 
may be a single character.

```latex
\def \foo [#1]#2{The first argument is ``#1'', the second one is ``#2''}
\foo{Hello}{Goodbye}
```

## Fonts

Originally intended to use its own font system, MetaFont. Computer Modern is the
default font system for TeX. These are high quality, scalable fonts. MetaFont is 
a complex font system. Most modern TeX compilers allow for the use of external 
font systems such as Truetype font (ttf) and OpenType font (otf).

## Category codes

When TeX parses an input, it assigns it a category code. TeX uses the character 
and its category to determine how to interpret it. These are used as primitive 
commands for common tasks. 

There are sixteen category codes:

- 0 = escape `\`
- 1 = group start `{`
- 2 = group end `}`
- 3 = math shift `$`
- 4 = alignment tab `&`
- 5 = end of line
- 6 = parameter for macros `#`
- 7 = math superscript `^`
- 8 = math subscript `_`
- 9 = ignored character
- 10 = space
- 11 = letters of the alphabet
- 12 = other character `.`, `1`, `:`
- 13 = active character `~`
- 14 = comment `%`
- 15 = invalid character `[DEL]`

## Keywords

TeX has 25 keywords. There are no backslashes before them. TeX interprets them 
depending on the mode.

```
at  dd    fill   mm   sp
by  depth filll  mu   spread
bp  em    height pc   to
cc  ex    in     plus true
cm  fil   minus  pt   width
```

## Primitive sequences

TeX has 325 primitive control sequences.


## References

- [TeX Primitive Control Sequences](http://webpages.charter.net/davidlha/trm.html)
