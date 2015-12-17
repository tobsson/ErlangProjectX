#!/bin/sh
exec erl \
    -pa ebin mochix/deps/*/ebin \
    -pa ebin wordval/deps/*/ebin \
    -pa ebin mochix/ebin \
    -pa ebin wordval/ebin \
    -boot start_sasl \
    -sname mochix_dev \
    -s mochix \
    -s reloader \
    -s projectx_app init \
    -s wordval_app init
