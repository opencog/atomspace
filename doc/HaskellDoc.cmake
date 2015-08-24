
EXECUTE_PROCESS(
    COMMAND stack haddock --extra-lib-dirs=${BINDIR}/opencog/haskell
    WORKING_DIRECTORY "${SRCDIR}/opencog/haskell"
)

EXECUTE_PROCESS(
    COMMAND mkdir ${BINDIR}/doc/html
)

EXECUTE_PROCESS(
    COMMAND mkdir ${BINDIR}/doc/html/haskell
)

EXECUTE_PROCESS(
    COMMAND find .stack-work/install -name "doc"
    COMMAND xargs cp -r -t ${BINDIR}/doc/html/haskell
    WORKING_DIRECTORY "${SRCDIR}/opencog/haskell"
)

