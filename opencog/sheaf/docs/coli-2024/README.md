
Submission to Computational Linguistics
---------------------------------------
https://cljournal.org/

My ORCID: https://orcid.org/0000-0002-2557-740X

CoLi LaTeX HOWTO
-----------------
* Style guideline: https://cljournal.org/style.html
* Copy `clv3.cls` to `/usr/share/texmf/tex/latex/`
* Run `sudo texhash`
* Copy `clv3.layout` to `~/.lyx/layouts/`
* Run reconfigure and restart LyX
* Select clv3 in Document->Settings->Document Class


Errors:
-------
* `Use of \x doesn't match its definition.`
   Document Settings -> PDF Properties -> disable Hyperref support
