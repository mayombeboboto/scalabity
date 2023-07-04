#!/bin/sh

erlc -o ./ebin ./src/*.erl && erl -pa ebin -config bsc.config