#!/bin/bash

exec erl -sname edts -pa `pwd`/lib/edts/ebin `pwd`/lib/webmachine/ebin `pwd`/lib/webmachine/deps/mochiweb/ebin -s edts_app









