SHELL := /usr/bin/env bash
MAKEFLAGS := --trace --warn-undefined-variables --print-directory -L

export PATH := $(PATH):build/

all: cabal test

cabal:
	cabal v2-build -j4

test: build/stanly build/stanly-test
	stanly-test --qc-max-success=2000 --color
	stanly -v < tests/polynomial.adi | tee >(grep "100000, 32, 5, 0" -q)
	stanly -v < tests/recursion.adi  | tee >(grep "3, 8, 21, 5040, 720, 1" -q)
	touch test

.PHONY: cabal 
