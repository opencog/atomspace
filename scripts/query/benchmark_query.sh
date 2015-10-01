# Small script to test the performance of the pattern matcher. It
# merely runs all the unit tests in query a certain number of times
# and output statistics about it.

# This script relies on https://github.com/nferraz/st

# Check unbound variables
set -u

# Debug trace
# set -x

# Number of times to run the unit tests
N=20

# Name of the build directory
BUILD_DIR_NAME=build

# Get the script directory
PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
UTEST_DIR="$PRG_DIR/../../$BUILD_DIR_NAME/tests/query"

#############
# Functions #
#############

# Get the real time in second (given the output of time command)
get_real_time() {
    grep "real" | cut -d' ' -f2
}

# Run all query unit tests
run_all_utests() {
    for ut in "$UTEST_DIR"/*UTest; do
        "$ut" > /dev/null
    done
}

########
# Main #
########

for i in $(seq 1 $N); do
    time -p run_all_utests 
done |& get_real_time | st | column -t
