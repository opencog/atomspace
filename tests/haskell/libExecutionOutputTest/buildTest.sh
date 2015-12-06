BIN_DIR=$1
SOURCE_DIR=$2

# libname=$(stack query | awk 'NR==2' | sed 's/://g'| sed 's/ //g')
# libver=$(stack query | awk 'NR==4' | sed 's/version: //g' | sed "s/'//g" | sed "s/ //g")
# Ugly hack to get the code to build.
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
