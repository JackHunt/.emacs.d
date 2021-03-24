#!/bin/bash

fnames[0]='.git'
fnames+=($(git ls-files))
printf "FNAME: %s\n" "${fnames[@]}"
