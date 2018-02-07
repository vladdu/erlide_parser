#! /bin/bash

chmod ug+x rebar3
./rebar3 escriptize
cp _build/default/bin/erlang_ls .
