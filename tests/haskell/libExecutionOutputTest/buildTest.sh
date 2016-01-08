BIN_DIR=$1
SOURCE_DIR=$2

libname="opencoglib"
libver="0.1.0.0"

if [ "$(id -u)" -ne 0 ]
then
    #Cleanup of last build if it exists
    rm "$SOURCE_DIR/lib$libname-$libver.so"
    rm "$SOURCE_DIR/a.out"

    # Build haskell bindings package.
    stack build --no-run-tests --extra-lib-dirs=${BIN_DIR}

    LIB=$(find . -name "*$libname*.so" | awk 'NR==1')

    cp $LIB "$SOURCE_DIR/lib$libname-$libver.so"
else
    echo "Can't run Haskell-Tests as root"
fi
