.SUFFIXES: .tex .pdf .Rnw

manchester: manchester.pdf 

manchester.pdf: manchester.Rnw manchester.bbl

manchester.bbl: ../bibliography/cities.bib
	biber manchester

.Rnw.tex:	
	R CMD Sweave $<

.tex.pdf:
	pdflatex $<
	pdflatex $<
