#! /bin/bash

wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s .

eval `./opam config env`
./opam update
eval `./opam config env`
opam switch create techelson 4.07.1
eval `./opam config env`
opam install dune menhir
eval `./opam config env`

./opam --version

make

make test