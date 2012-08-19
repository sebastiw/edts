#!/bin/bash

exec erl -sname edts -pa `pwd`/../edts/ebin `pwd`/../webmachine/ebin `pwd`/../webmachine/deps/mochiweb/ebin -s edts_app









