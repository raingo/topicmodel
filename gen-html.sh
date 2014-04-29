#!/bin/sh
# vim ft=sh

filename=$1

if [ -z $filename ]
then
    echo $0, filename
    exit
fi
#name=report
name=report

# R CMD BATCH report.gen.R
Rscript -e "library('knitr')" -e "knit('$name.Rmd')"
pandoc -s -S -i -t slidy --mathjax $name.md -o $name.html
gnome-open $name.html


#pandoc -s -S -i -t slidy --mathjax refer.md -o report.html
#pandoc -s -S -i -t s5 --mathjax report.md -o report.html
#pandoc -s -S -i -t dzslides --mathjax report.md -o report.html
#pandoc -s -S -i -t beamer --mathjax report.md -o report.tex
