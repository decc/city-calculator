.SUFFIXES: .tex .pdf .Rnw

manchester: manchester.pdf

manchester.pdf: manchester.Rnw ../bibliography/cities.bib

.Rnw.tex:	
	R CMD Sweave $<

.tex.pdf:
	pdflatex $<
	pdflatex $<
