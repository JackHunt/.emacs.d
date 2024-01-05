#!/bin/bash

untracked_files=$(git clean -x -n -d)

if [ -z "$untracked_files" ]; then
    echo "Nuffin 2 yeet."
    exit 0
fi

echo "Nuffin 2 delete m8:"
echo "$untracked_files"

read -p "Yeet dis shyte? (y/N) " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    git clean -f -x -d
fi
