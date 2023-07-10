#!/usr/bin/env bash

python3 -m pip install \
  python-language-server[all] \
  pyls-black \
  pyls-isort \
  pyls-mypy \
  pyls-flake8 \
  debugpy
