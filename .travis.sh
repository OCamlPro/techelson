#! /bin/bash

if [[ "$TRAVIS_OS_NAME" == "linux" ]] ; then
    sudo apt-get update -qq
    sudo apt-get install -y -qq libgmp-dev pandoc # ocaml ocaml-native-compilers

    # do this in a second step to only install libsecp256k1-dev libsecp256k1-0
    # for ubuntu, these packages are not available in trusty
    sudo add-apt-repository "deb http://archive.ubuntu.com/ubuntu artful main universe"
    sudo apt-get update -qq
    sudo apt-get install -y -qq \
         libsecp256k1-dev libsecp256k1-0 libsodium-dev libssl-dev \
         bubblewrap libev-dev libhidapi-dev
fi

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