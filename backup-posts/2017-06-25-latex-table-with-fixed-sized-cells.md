---
title: LaTeX Table with Fixed Size Cells
---

It is easy to restrict the width of a LaTeX table cell, but challenging to do so 
for a table cell's height.

array – Extending the array and tabular environments


`\def <command> <parameter-text>{<replacement-text>}`

`\def` allows us to define new
command is `\` followed by one letter and single 


define a new column specifier for a table.
`\newcolumntype{x}{>{〈some declarations〉}{c}<{〈some more declarations〉}}`

`\mbox{}` to prevent line break.
  
`\begin{tabular}[pos]{table spec}`

`\rule`
`\vrule` vertical rule
`\hrule` horizontal rule

paragraph column with text vertically aligned at the top

p type column  
  By default, if the text in a column is too wide for the page, LaTeX won’t automatically wrap it. Using p{'width'} you can define a special type of column which will wrap-around the text as in a normal paragraph. You can pass the width using any unit supported by LaTeX, such as 'pt' and 'cm', or command lengths, such as \textwidth. You can find a list in chapter Lengths.


```latex
\documentclass{article}

\usepackage{array}

\begin{document}


\def\mystrut(#1,#2){\vrule height #1 depth #2 width 0pt}

\newcolumntype{C}[1]{%
   >{\mystrut(5ex,0ex)\centering}%
   p{#1}%
   <{}}  

\def\cell(#1,#2,#3){\mbox{#1} \newline \mbox{#2} \newline \mbox{#3} \newline}

\begin{tabular}{|C{3cm} | C{3cm} | C{3cm} |} 
\hline 
\cell(Short Text,,) & \cell(,Middle text,) & \cell(a,b,c) \tabularnewline \hline
\cell(,middle,) & \cell(top,,bottom) & \cell(a,,) \tabularnewline \hline
\cell(,,) & \cell(,,) & \cell(,,) \tabularnewline \hline  
\end{tabular}   

\end{document}
```

https://tex.stackexchange.com/questions/35515/how-can-i-reduce-table-row-height

[](http://ftp.yzu.edu.tw/CTAN/macros/latex/required/tools/array.pdf)

[](https://tex.stackexchange.com/questions/40977/confused-with-tex-terminology-height-depth-width)

https://tex.stackexchange.com/questions/50352/inserting-a-small-vertical-space-in-a-table
