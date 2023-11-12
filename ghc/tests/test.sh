#!/usr/bin/env bash
green () { printf "\x1b[32m%s\x1b[0m\n" "$1"; }
red   () { printf "\x1b[31m%s\x1b[0m\n" "$1"; }
stanly="cabal v2-run stanly -- "            #      stanly="./dist-newstyle/build/x86_64-linux/ghc-9.2.8/stanly-0.1.0.0/x/stanly/build/stanly/stanly"
stanly_test="cabal v2-run stanly-test -- "  # stanly_test="./dist-newstyle/build/x86_64-linux/ghc-9.2.8/stanly-0.1.0.0/t/stanly-test/build/stanly-test/stanly-test"
$stanly_test --qc-max-success=2000
$stanly -v < test/polynomial.adi | grep "100000, 32, 5, 0"       -q || { printf "%s failed\n" "$(red 'Error: polynomial')"  >&2; exit 1; }
$stanly -v < test/recursion.adi  | grep "3, 8, 21, 5040, 720, 1" -q || { printf "%s failed\n" "$(red 'Error: recursion' )"  >&2; exit 1; }
printf "%s passed\n" "$(green 'file tests')"