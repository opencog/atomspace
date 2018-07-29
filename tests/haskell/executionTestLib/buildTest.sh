BIN_DIR=$1
SOURCE_DIR=$2

libname=opencoglib
libver=$(stack query locals opencoglib | awk 'NR==2' | sed 's/version: //g' | sed "s/'//g" | sed "s/ //g")

if [ "$(id -u)" -ne 0 ]
then
    #Cleanup of last build if it exists
    artifact_1="$SOURCE_DIR/../haskellTest/lib$libname-$libver.so"
    artifact_2="$SOURCE_DIR/a.out"
    if [ -e "$artifact_1" ]; then rm "$artifact_1"; fi
    if [ -e "$artifact_2" ]; then rm "$artifact_2"; fi

    # Build haskell bindings package.
    stack build --no-run-tests --extra-lib-dirs=${BIN_DIR}

    LIB=$(find . -name "*$libname*.so" | awk 'NR==1')

    cp $LIB "$SOURCE_DIR/../haskellTest/lib$libname-$libver.so"
else
    echo "Can't run Haskell-Tests as root"
fi
