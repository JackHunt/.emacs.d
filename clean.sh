#!/bin/bash

fnames_keep[0]='.git'
fnames_keep+=($(git ls-files))

fnames_all=($(ls -a))

for f in "${fnames_all[@]}"
do
    if [[ ! " ${fnames_keep[@]} " =~ " ${f} " ]]; then
        echo "Deleting: $f"
        rm -rf "$f"
    fi
done
