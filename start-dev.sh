#!/bin/sh
exec erl \
    -pa ebin mochix/deps/ibrowse/ebin \
    -pa ebin mochix/deps/jiffy/ebin \
    -pa ebin mochix/deps/mochiweb/ebin \
    -pa ebin wordval/deps/couchbeam/ebin \
    -pa ebin mochix/ebin \
    -pa ebin wordval/ebin \
    -boot start_sasl \
    -sname mochix_dev \
    -s mochix \
    -s reloader
