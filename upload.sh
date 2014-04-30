#!/bin/sh
# vim ft=sh

tarfile=bst512.tar.bz2
tar cfa $tarfile *.png report.html style.css figure/
scp $tarfile f07:tmp/
ssh f07 << EOF
cd tmp/
mkdir -p /u/yli/www/bst512
tar xfa $tarfile -C /u/yli/www/bst512
EOF
rm $tarfile
