#!/bin/sh
git submodule update --init
make
(cd lib/edts && ./rebar eunit skip_deps=true) &&
emacs -Q --batch --eval "(add-to-list 'load-path \"/home/thomas.jarvstrand/klarna/erlang/otp/lib/tools/emacs\")" -l edts-start.el -f ert-run-tests-batch-and-exit
