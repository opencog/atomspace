#!/usr/bin/env python2

# Given a log file, and a FCS handle, filtered that log file to retain
# only the iterations leading to that FCS.

import sys
import re

#############
# Constants #
#############

# Usage message
usage = "Usage: " + sys.argv[0] + " FCSHANDLE LOGFILE"

# Useful regex
timestamp_re = r'\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}:\d{3}\]'
debug_re = r'\[DEBUG\]'
bc_re = r'\[BackwardChainer\]'
iter_re = r'Iteration (\d+)'
iteration_re = r'{} {} {} {}'.format(timestamp_re, debug_re, bc_re, iter_re)
iteration_cre = re.compile(iteration_re)
from_re = r'from:'
from_cre = re.compile(from_re)
to_re = r'to:'
to_cre = re.compile(to_re)
handle_re = r'\) ; (\[\d+\]\[\d+\])'
handle_cre = re.compile(handle_re)

########
# Main #
########

if len(sys.argv) != 3:
    print usage
    exit(1)

fcs_handle = sys.argv[1]
logfile = sys.argv[2]

# Build map from iteration to (from, to) pair
i2ft = dict()
src = ''
for l in open(logfile):
    ls = l.rstrip()
    
    # Extract iteration
    m = iteration_cre.match(ls)
    if m:
        iteration = int(m.group(1))
        continue
    
    # Extract from/to fcs handle
    m = from_cre.match(ls)
    if m:
        src = 'from'
        continue
    m = to_cre.match(ls)
    if m:
        src = 'to'
        continue
    m = handle_cre.match(ls)
    if m:
        if src == 'from':
            from_handle = m.group(1)
            continue
        if src == 'to':
            to_handle = m.group(1)
            i2ft[iteration] = (from_handle, to_handle)
            if fcs_handle in to_handle:
                fcs_handle_iteration = iteration
                break
            src = ''            # reset src to not overwrite i2ft
                                # before the end of the iteration
            continue

# Filter i2ft to contain only traces to fcs_handle
i2ft_trace = dict()
iteration = fcs_handle_iteration
while iteration != 0:
    i2ft_trace[iteration] = i2ft[iteration]
    for i in range(iteration):
        if (i in i2ft and i2ft[i][1] == i2ft[iteration][0]):
            iteration = i
            break
    if iteration != i:
        iteration = 0

# Display all iterations leading to the FCS
iteration = -1
for l in open(logfile):
    ls = l.rstrip()

    # Extract iteration
    m = iteration_cre.match(ls)
    if m:
        iteration = int(m.group(1))
    
    if iteration == 0 or iteration in i2ft_trace:
        print ls
