#!/bin/sh

erlc -o ./ebin ./src/*.erl && erl -sname n3@localhost -pa ebin -config bsc.config