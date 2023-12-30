#!/usr/bin/env bash

red() { printf "\e[31m%s\e[0m\n" "$1"; }
green() { printf "\e[32m%s\e[0m\n" "$1"; }

error() { 
    red "Error: $1"
    exit 1
}

export PATH=$PATH:build/

for file in tests/*.adi
do 
    # shellcheck disable=SC2059
    cmd="$(printf "$(sed -n -E 's|^// RUN:\s*(.*)|\1|p' "$file")" "$file" "$file")"
    echo "$cmd"
    bash -c "$cmd"
done

error "Need to fix test.sh to show red error message when FileCheck has nonzero return status."