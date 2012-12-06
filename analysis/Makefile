.SUFFIXES: .tex .pdf .Rnw

manchester: manchester.pdf 

manchester.pdf: manchester.Rnw manchester.bbl ../R/city-tool.R

manchester.bbl: ../bibliography/cities.bib
	biber manchester

.Rnw.tex:	
	Rscript  -e "library(knitr); knit('$<')"

.tex.pdf:
	pdflatex $<
	pdflatex $<
