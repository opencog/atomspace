
EXECUTE_PROCESS(
    COMMAND cabal configure --builddir=${BINDIR}
    OUTPUT_QUIET
    ERROR_VARIABLE out
)

IF(out MATCHES "(.)*'hackage.haskell.org' does not exist(.)*")
    MESSAGE(STATUS "Updating cabal package list...")
    EXECUTE_PROCESS(
        COMMAND cabal update
    )
ENDIF(out MATCHES "(.)*'hackage.haskell.org' does not exist(.)*")

EXECUTE_PROCESS(
    COMMAND cabal install -v0 --builddir=${BINDIR}/dist --extra-lib-dirs=${BINDIR}
    OUTPUT_QUIET
)

