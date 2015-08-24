
#Compile package to object-code so it is loaded, not interpreted,
#on futures uses of ghci (faster).
EXECUTE_PROCESS(
    COMMAND echo ":q"
    COMMAND stack ghci --ghc-options=-fobject-code
                       --ghc-options=-L${BIN_DIR}
                       --verbosity=error
    OUTPUT_QUIET
)
