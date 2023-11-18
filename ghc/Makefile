SHELL := /usr/bin/env bash
MAKEFLAGS := --trace --warn-undefined-variables --print-directory -L

export PATH := $(PATH):build/

all: cabal test

cabal:
	cabal v2-build -j4

test: cabal
	stanly-test --qc-max-success=2000 --color
	./tests/test.sh


.PHONY: cabal test