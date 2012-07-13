#!/bin/zsh
ocamlc -c $* 2>&1 | perl -pi -e "s/:\\n/: /g" >&2
