#!/bin/sh
fn="t/baseline/$1-baseline"
./test-suite $1 | sed -E 's/(#[[:digit:]]+l)/#PTR#/g' | sed 's/[ \t]*$//' > $fn

