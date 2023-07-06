#!/bin/sh

erlc -o ./ebin ./src/*.erl && erl -sname n4@localhost -pa ebin -config bsc.config