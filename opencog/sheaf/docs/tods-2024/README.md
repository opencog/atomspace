
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

  PDFlatex uses latin9 so does luatex but xetex does not
  configure panel: Language: Enclding change from default to utf8

  Ah hah:
```
\usepackage{listings}
\renewcommand{\lstlistingname}{\inputencoding{latin9}Listing}
```
Go to listings, change from listing to minted  Nope. Doesnt; fix it.
Ahh
\inputencoding{latin9}\begin{lstlisting}[basicstyle={\footnotesize\sffamily},tabsize=3]

/usr/share/texlive/texmf-dist/tex/latex/listings/listings.sty
\lst@Key{inputencoding}\relax{\def\lst@inputenc{#1}}

Go to listings, set `inputencoding=utf8`  No not enough
switches input encoding before the listing.
exporting as luatex instead of pdflatex avoids this.

Go to PDF properties, disable "Use Hyperref support"
But lutatex still stries to load it with different options.

acmart-primary/acmart.cls:\RequirePackage[bookmarksnumbered,unicode]{hyperref}

Edit above change to unicode=true and copy into place.

```
LaTeX Warning: Unused global option(s):
    [format=acmsmall].
```

* Abstract must come before title!!

* Glyphs
  Missing character: There is no Ï (U+00CF) in font zptmcm7y!
  grep $'\u00CF'








TODO:
-----
* rename file to todssubmission.pdf
