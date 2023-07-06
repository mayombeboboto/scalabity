#!/bin/sh

erlc -o ./ebin ./src/*.erl && erl  -sname n2@localhost -pa ebin -config bsc.config