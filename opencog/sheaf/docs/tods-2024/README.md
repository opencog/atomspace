
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
* Custom config `acmsmall`
  See https://tex.stackexchange.com/questions/481052/lyx-choose-option-from-cls-file

* Place title after authors, abstract and ACM classifications. That's
  because LyX issues \maketitle right after title, and acmart expects
  all the other stuff to come beforehand.

vi HOWTO
--------
Search for utf8 char: `/ ctrl-V u 00xx`

Errors:
------
* `LaTeX Error: Command '\Bbbk' already defined.`
  Solution: stop loading amssymb See
  https://tex.stackexchange.com/questions/564490/command-bbbk-already-defined-ol-bbbk-mathordamsb7c-when-i-try-t

* Missing integral sign in footnote.
  Solution: load esint in Math Options config panel!

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

Maybe 
\RequirePackage[T1]{fontenc}
should be 
\RequirePackage[TU]{fontenc}

\usepackage[T1]{fontenc}

0x4d 0x69 0x73 0x73 0x69 0x6e 0x67 0x20 0x63 0x68 0x61 0x72 0x61 0x63 0x74 0x65 0x72 0x3a 0x20 0x54 0x68 0x65 0x72 0x65 0x20 0x69 0x73 0x20 0x6e 0x6f 0x20 0xb9 0x20 0x69 0x6e 0x20 0x66 0x6f 0x6e 0x74 0x20 0x7a 0x70 0x74 0x6d 0x63 0x6d 0x37 0x79 0x21

Missing character: There is no 0xb9 in font zpt






TODO:
-----
* rename file to todssubmission.pdf
