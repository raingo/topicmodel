#!/bin/sh
# vim ft=sh

fullfile=$1

if [ -z $fullfile ]
then
    echo $0, filename
    exit
fi
#name=report
filename=$(basename "$fullfile")
extension="${filename##*.}"
name="${filename%.*}"

# R CMD BATCH report.gen.R
Rscript -e "library('knitr')" -e "knit('$name.Rmd')"
#pandoc -s -S -i -t slidy -c style.css -V duration:45 --mathjax $name.md -o $name.html
#pandoc -s -S -t slidy -c style.css -V duration:45 --mathjax $name.md -o $name.html
pandoc -S -t slidy -c style.css -V duration:45 --mathjax $name.md -o $name.html
#pandoc -S -t revealjs -c style.css -V duration:45 --mathjax $name.md -o $name.html

if [ -z "`pgrep view-html.sh`" ]
then
    firefox $name.html &
    ./view-html.sh $name.html
fi


#pandoc -s -S -i -t slidy --mathjax refer.md -o report.html
#pandoc -s -S -i -t s5 --mathjax report.md -o report.html
#pandoc -s -S -i -t dzslides --mathjax report.md -o report.html
#pandoc -s -S -i -t beamer --mathjax report.md -o report.tex
