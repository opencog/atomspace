
EXECUTE_PROCESS(
    COMMAND cabal configure
    OUTPUT_QUIET
    ERROR_VARIABLE out
)

IF(out MATCHES "(.)*'hackage.haskell.org' does not exist(.)*")
    MESSAGE(STATUS "Updating cabal package list...")
    EXECUTE_PROCESS(
        COMMAND cabal update
    )
ENDIF(out MATCHES "(.)*'hackage.haskell.org' does not exist(.)*")
