#!/bin/bash
BIN_DIR=$1

ghcver="$(stack ghc -- --version)"

if [[ "$ghcver" == *8.0.2* ]]
then
    echo "Correct GHC version installed."
else
    echo "Wrong GHC version installed. Running stack setup."
    stack setup
fi

if [ "$(id -u)" -ne 0 ];
then
  # Build haskell bindings package.
  stack build --extra-lib-dirs=${BIN_DIR}
fi
