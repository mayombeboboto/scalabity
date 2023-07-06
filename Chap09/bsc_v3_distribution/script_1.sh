#!/bin/sh

erlc -o ./ebin ./src/*.erl && erl  -sname n1@localhost -pa ebin -config bsc.config