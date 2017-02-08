#!/usr/bin/env bash
./rebar3 compile
erl -pa ./_build/default/lib/erlang_postmarkapp/ebin/ -pa ./_build/default/lib/jsx/ebin/