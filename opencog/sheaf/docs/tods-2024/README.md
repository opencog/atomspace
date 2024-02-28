
ACM Transactions on Database Systems
------------------------------------
https://mc.manuscriptcentral.com/tods
Manuscript ID: TODS-2024-0011

* 24 February 2024
  Submitted to ACM Transactions on Database Systems

* 28 Feb revised submission per author guidelines.


ACM Latex HOWTO
---------------
* Copy `acmart.cls` to `/usr/share/texmf/tex/latex/`
* Run `sudo texhash`
* Copy `acmart.layout` to `~/.lyx/layouts/`
* Run reconfigure and restart LyX
* Select acmart in Document->Settings->Document Class
  Custom config `acmsmall`
  See https://tex.stackexchange.com/questions/481052/lyx-choose-option-from-cls-file

* Place title after authors, abstract and ACM classifications. That's
  because LyX issues \maketitle right after title, and acmart expects
  all the other stuff to come beforehand.

* Holy Cow: There's an `acmart.lyx` article template that would have
  resolved all of the above! Yow!


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
But lutatex still tries to load it with different options.

acmart-primary/acmart.cls:\RequirePackage[bookmarksnumbered,unicode]{hyperref}

Edit above change to unicode=true and copy into place.

* Solution: Abstract must come before title!!
  That's because Lyx issues a \maketitle too early and acmart chokes
  cause it happens before \begin{document}

* Glyphs
  Missing character: There is no Ï (U+00CF) in font zptmcm7y!
  Solution: Document Settings -> Fonts -? Math: "Class Default"
      instead of "automatic".

  The bad one has:
     \usepackage{mathptmx}
  The good one has
     \renewcommand{\rmdefault}{ptm}


TODO:
-----
* rename file to todssubmission.pdf
