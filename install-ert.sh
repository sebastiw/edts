#!/bin/sh
mkdir -p "elisp/ert"
cd "elisp/ert"
wget -nv https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert.el
wget -nv https://github.com/mirrors/emacs/raw/master/lisp/emacs-lisp/ert-x.el
