BIN_DIR=$1

if [ "$(id -u)" -ne 0 ]
then
  # Build haskell bindings package.
  stack test --no-run-tests --extra-lib-dirs=${BIN_DIR}
else
    echo "Can't run Haskell-Tests as root"
fi
