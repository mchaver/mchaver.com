https://tex.stackexchange.com/questions/93797/how-to-rotate-text-inline
https://tex.stackexchange.com/questions/38577/vertical-chinese-text-with-xetex
https://tex.stackexchange.com/questions/46199/texshop-doesnt-remember-file-encoding
https://tex.stackexchange.com/questions/18157/rotating-a-letter
https://www.preining.info/blog/2014/12/writing-japanese-in-latex-part-1-introduction/
https://nablux.net/tgp/weblog/2013/03/22/how-typeset-japanese-using-xelatex/

https://tex.stackexchange.com/questions/271300/using-platex-with-texshop-without-setting-it-as-default
https://texwiki.texjp.org/?upTeX%2CupLaTeX


sudo /Library/TeX/texbin/tlmgr update --self --all --reinstall-forcibly-removed
https://tex.stackexchange.com/questions/55437/how-do-i-update-my-tex-distribution
https://tex.stackexchange.com/questions/142554/command-line-pdflatex-vs-texshop

https://texwiki.texjp.org/?upTeX%2CupLaTeX

/Library/TeX/texbin/dvipdfmx vertical-text.dvi 

```latex
% !TEX TS-program = platex
%\documentclass[dvipdfmx]{utarticle} % dvipdfmx driver option (for color) required, for post-processing of dvipdfmx
%\usepackage{color} % not required for this example...

\documentclass{article}
\begin{document}
\LaTeX で日本語を書きましょう！
\end{document}
```


/Library/TeX/texbin/uplatex vertical-text.tex

https://tex.stackexchange.com/questions/374838/why-do-uplatex-display-chinese-text-with-japanese-punctations-instead-of-chinese/374897#374897
