.SUFFIXES: .tex .pdf .Rnw

city: city.pdf 

city.pdf: city.Rnw city.bbl ../R/city-tool.R

city.bbl: ../bibliography/cities.bib
	biber city

.Rnw.tex:	
	Rscript  -e "library(knitr); knit('$<')"

.tex.pdf:
	pdflatex $<
	pdflatex $<
