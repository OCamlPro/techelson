#! /bin/bash

sudo apt-get update -y -qq

export OPAMYES=1
wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
yes "" | sh install.sh

eval $(opam config env)

opam init

eval $(opam config env)

opam switch create techelson 4.07.1

eval $(opam config env)

opam update
opam install dune menhir zarith ptime

opam --version

make

make test