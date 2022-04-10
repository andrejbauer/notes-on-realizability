#!/bin/bash
cd notes && \
latexmk mgs-exercises.tex && \
rsync -l -e ssh -r -v notes-on-realizability.pdf andrej@lisa.andrej.com:/var/www/andrej.com/zapiski/MGS-2022/
