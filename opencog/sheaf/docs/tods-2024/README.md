
24 February 2024

Submitted to ACM Transactions on Database Systems

Manuscript ID: TODS-2024-0011

https://mc.manuscriptcentral.com/tods

ACM Latex HOWTO
---------------
* Copy *cls to `/usr/share/texmf/tex/latex/`
* Run `sudo texhash`
* Copy *layout to `~/.lyx/layouts/`
* Run reconfigure and restart LyX
* Select amsart in document config
* Custom config `format=acmsmall`
  See https://tex.stackexchange.com/questions/481052/lyx-choose-option-from-cls-file

vi HOWTO
--------
Search for utf8 char: `/ ctrl-V u 00xx`

Errors:
------
* `LaTeX Error: Command '\Bbbk' already defined.`
  Solution: stop loading amssymb See
  https://tex.stackexchange.com/questions/564490/command-bbbk-already-defined-ol-bbbk-mathordamsb7c-when-i-try-t

* `LaTeXError: Missing \begin{document}.`
  Remove `\usepackage[latin9]{inputenc}`
  https://tex.stackexchange.com/questions/527719/latex-error-using-lyx-missing-begindocument
  Where is this coming from?
  /usr/share/lyx
  /usr/share/texlive/


TODO:
-----
* rename file to todssubmission.pdf
