
FIND_PROGRAM(CABAL_EXECUTABLE cabal)

IF (CABAL_EXECUTABLE)
    EXECUTE_PROCESS(
        COMMAND cabal list nothing
        OUTPUT_QUIET
        ERROR_VARIABLE out
    )

    IF(out MATCHES "(.)*'hackage.haskell.org' does not exist(.)*")
        MESSAGE(STATUS "Updating cabal package list...")
        EXECUTE_PROCESS(
            COMMAND cabal update
        )
    ENDIF(out MATCHES "(.)*'hackage.haskell.org' does not exist(.)*")

    SET(CABAL_FOUND TRUE)
ELSE (CABAL_EXECUTABLE)
	SET(CABAL_FOUND FALSE)
ENDIF (CABAL_EXECUTABLE)

