#!/bin/sh
BIN_DIR=$1

ghcver="$(stack --allow-different-user ghc -- --version)"

case "$ghcver" in
    *"8.0.2"*)
            echo "Correct GHC version installed."
            ;;
    *)
	    echo "Wrong GHC version installed. Running stack setup."
	    stack setup --allow-different-user
	    ;;
esac

if [ "$(id -u)" -ne 0 ];
then
  # Build haskell bindings package.
  stack build --allow-different-user --extra-lib-dirs=${BIN_DIR}
fi
