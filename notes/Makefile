MAIN=notes-on-realizability.tex

default:
	pdflatex -interaction=nonstopmode $(MAIN)
	makeindex $(MAIN).nlo -s nomencl.ist -o $(MAIN).nls
	makeindex $(MAIN)
	biber $(MAIN)
	pdflatex $(MAIN)
	makeglossaries $(MAIN)
	pdflatex $(MAIN)
