/**
 * PatternUtils.h
 *
 * Utilities for mangling graphs
 *
 * Copyright (C) 2009, 2014 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Created by Linas Vepstas February 2008
 */

#ifndef _OPENCOG_PATTERN_UTILS_H
#define _OPENCOG_PATTERN_UTILS_H

#include <set>
#include <vector>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
#include "Pattern.h"

namespace opencog {

// Return true iff the clause is evaluatable.
bool can_evaluate(const Handle& clause);

// Return true iff the clause is constant. That is iff
//
// 1. clause is not evaluatable
//
// 2. and none of its free variables are in vars, as only the
//    variables in vars should be interpreted as variables, the others
//    should be interpreted as constants.
bool is_constant(const HandleSet& vars, const Handle& clause);

// Return true iff the clause is a "black box" evaluatable.
bool is_black_box(const Handle& clause);

// See C file for description
void get_connected_components(const HandleSet& vars,
                              const HandleSeq& clauses,
                              HandleSeqSeq& compset,
                              HandleSetSeq& compvars);

void get_bridged_components(const HandleSet& vars,
                            const PatternTermSeq& clauses,
                            const PatternTermSeq& opts,
                            HandleSeqSeq& compset,
                            HandleSetSeq& compvars);

} // namespace opencog

#endif // _OPENCOG_PATTERN_UTILS_H
