BIN_DIR=$1
SOURCE_DIR=$2

libname="opencoglib"
libver="0.1.0.0"

if [ "$(id -u)" -ne 0 ]
then
    # Build haskell bindings package.
    stack build --no-run-tests --extra-lib-dirs=${BIN_DIR}

    LIB=$(find . -name "*$libname*.so" | awk 'NR==1')

    rm "$SOURCE_DIR/lib$libname-$libver.so"
    cp $LIB "$SOURCE_DIR/lib$libname-$libver.so"
else
    echo "Can't run Haskell-Tests as root"
fi
