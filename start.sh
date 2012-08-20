#!/bin/bash

###############################################################################
### Startup-script
### Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
###
### This file is part of EDTS.
###
### EDTS is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.
###
### EDTS is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with EDTS. If not, see <http://www.gnu.org/licenses/>.
###############################################################################

if [ -z $1 ];
then
    ERL=`which erl`
else
    ERL=$1
fi

exec $ERL -sname edts -pa `pwd`/lib/edts/ebin `pwd`/lib/webmachine/ebin `pwd`/lib/webmachine/deps/mochiweb/ebin -s edts_app

