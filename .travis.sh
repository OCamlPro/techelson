#! /bin/bash

sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

eval $(opam config env)
opam update
eval $(opam config env)
opam switch create techelson 4.07.1
eval $(opam config env)
opam install dune menhir zarith ptime
eval $(opam config env)

opam --version

make

make test