#!/bin/bash
# This script is used to merge separate lcov output from each test.
# Once they are in a single file, we create the html output with genhtml
# XXX FIXME .. there is no reason to merge them. This just east CPU
# time...
BASEDIR=.
rm -r ${BASEDIR}/lcov

ifiles=""
for f in ${BASEDIR}/coverage/*.info; do
    ifiles="$ifiles -a $f"
done
echo "*** Combining all lcov .info files into single file"
echo Command is: lcov --directory ${BASEDIR} $ifiles --output-file ${BASEDIR}/coverage/alltemp.info
time lcov --directory ${BASEDIR} $ifiles --output-file ${BASEDIR}/coverage/alltemp.info

# ------------
echo "*** Removing coverage info for non-AtomSpace files"
# Get the name of the current dir
BUILD_DIR=${PWD##*/}
echo Command is: lcov --directory ${BASEDIR} --output-file ${BASEDIR}/coverage/all.info \
    --remove ${BASEDIR}/coverage/alltemp.info \
        /usr/include/\* \
        ${BUILD_DIR}/tests/\*
lcov --directory ${BASEDIR} --output-file ${BASEDIR}/coverage/all.info \
    --remove ${BASEDIR}/coverage/alltemp.info \
        /usr/include/\* \
        ${BUILD_DIR}/tests/\*
rm ${BASEDIR}/coverage/alltemp.info

# ------------
echo "*** Creating lcov html summary"
echo Command is: genhtml -s -o ${BASEDIR}/lcov --demangle-cpp --num-spaces 4 --title "AtomSpace Coverage Analysis" ${BASEDIR}/coverage/all.info
time genhtml -s -o ${BASEDIR}/lcov --demangle-cpp --num-spaces 4 --title "AtomSpace Coverage Analysis" ${BASEDIR}/coverage/all.info
echo "Moving coverage files to ${BASEDIR}/coverage/old"
mkdir -p ${BASEDIR}/coverage/old
for f in ${BASEDIR}/coverage/*.info; do
    mv $f ${BASEDIR}/coverage/old
done
echo "*** All done!"
echo "*** Coverage reports are in ${BASEDIR}/lcov; use web browser to view."
