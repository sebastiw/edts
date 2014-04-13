#!/bin/sh
mkdir -p elisp/erlang-mode
cd elisp/erlang-mode
wget https://raw.githubusercontent.com/erlang/otp/1ba34e263a71760d6e472a2800f5dc194847aad6/lib/tools/emacs/erlang.el -O erlang.el
wget https://raw.githubusercontent.com/erlang/otp/1ba34e263a71760d6e472a2800f5dc194847aad6/lib/tools/emacs/erlang-skels.el -O erlang-skels.el
