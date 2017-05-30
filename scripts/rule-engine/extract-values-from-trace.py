#!/usr/bin/env python2

# Given a trace extracted from a log file prints all FCS values. This
# is convenient to feed and-BIT fitness functions like
# AndBITFitness::Trace.


import sys
import re

#############
# Constants #
#############

# Usage message
usage = "Usage: " + sys.argv[0] + " LOG_TRACE_FILE"

# Useful regex
timestamp_re = r'\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}:\d{3}\]'
debug_re = r'\[DEBUG\]'
ure_re = r'\[URE\]'
select_andbit_re = r'Selected and-BIT for expansion:'
from_re = r'({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, select_andbit_re)
from_cre = re.compile(from_re)
expand_andbit_re = r'Expanded forward chainer strategy:'
select_bn_re = r'Selected BIT-node for expansion:'
inter_re = r'({} )?{} {} {}'.format(timestamp_re, debug_re, ure_re, select_bn_re)
inter_cre = re.compile(inter_re)
handle_re = r'\) ; \[(\d+)\]\[\d+\]'
handle_cre = re.compile(handle_re)

########
# Main #
########

if len(sys.argv) != 2:
    print usage
    exit(1)

logtracefile = sys.argv[1]

# Display all selected and-BITs for expansion
src = ''
for l in open(logtracefile):
    ls = l.rstrip()

    # Extract from fcs handle
    m = from_cre.match(ls)
    if m:
        src = 'from'
        continue
    m = inter_cre.match(ls)
    if m:
        src = ''
        continue
    m = handle_cre.match(ls)
    if m:
        if src == 'from':
            print m.group(1)
            continue
