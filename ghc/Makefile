SHELL := /usr/bin/env bash
MAKEFLAGS := --trace --warn-undefined-variables --print-directory -L

export PATH := $(PATH):build/

all: format cabal test

format:
	fourmolu --unicode always --mode inplace src/Stanly/*.hs app/*.hs tests/*.hs

cabal: format
	cabal v2-build -j4

test: cabal
	stanly-test --qc-max-success=2000 --color
	./tests/test.sh

.PHONY: cabal test format