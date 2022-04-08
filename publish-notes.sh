#!/bin/bash
cd notes && latexmk notes-on-realizability.tex && \
rsync -l -e ssh -r --delete -v notes-on-realizability.pdf andrej@lisa.andrej.com:/var/www/andrej.com/zapiski/MGS-2022/
