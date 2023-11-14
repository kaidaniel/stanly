#!/usr/bin/env bash

export PATH=$PATH:build/

for file in tests/*.adi
do 
    # shellcheck disable=SC2059
    cmd="$(printf "$(sed -n -E 's|^// RUN:\s*(.*)|\1|p' "$file")" "$file" "$file")"
    echo "$cmd"
    bash -c "$cmd"
done