BIN_DIR=$1

if [ "$(id -u)" -ne 0 ];
then
  # Build haskell bindings package.
  stack build --extra-lib-dirs=${BIN_DIR}
fi
