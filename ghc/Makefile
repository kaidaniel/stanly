SHELL := /usr/bin/env bash -O globstar
MAKEFLAGS := --trace --warn-undefined-variables --print-directory -L

export PATH := $(PATH):build/

all: format cabal test

format:
	./haskell-unicode src/**/*.hs tests/*.hs app/*.hs
	fourmolu --column-limit 90 --unicode always --mode inplace src/**/*.hs app/*.hs tests/*.hs

cabal: format
	cabal v2-build -j8

test: cabal
	stanly-test --qc-max-success=200 --color
	./tests/test.sh

.PHONY: cabal test format