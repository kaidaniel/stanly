#!/usr/bin/env bash

nok=0
nfailure=0
red() { printf "\e[31m%s\e[0m\n" "$1"; }
green() { printf "\e[32m%s\e[0m\n" "$1"; }
failure() { red "✘ $1"; nfailure=$((nfailure+1)); }
ok() { green "✔ $1"; nok=$((nok+1)); }

export PATH=$PATH:build/
files="tests/*.adi"

for file in $files
do 
    # shellcheck disable=SC2059
    cmd="$(printf "$(sed -n -E 's|^// RUN:\s*(.*)|\1|p' "$file")" "$file" "$file")"
    if bash -c "$cmd"; then f=ok; else f=failure; fi
    $f "$cmd"
done

if [[ nfailure -eq 0 ]]; then g=green; else g=red; fi
$g "$files: $nok ok, $nfailure failures"