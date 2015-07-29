/*
 * FuzzyPatternMatch.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Leung Man Hin <https://github.com/leungmanhin>
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
 */

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/bind/PatternLink.h>
#include <time.h>

#include "FuzzyPatternMatchCB.h"
#include "FuzzyPatternMatch.h"

using namespace opencog;

//#define DEBUG

/**
 * Implement the "cog-fuzzy-match" scheme primitive.
 * It uses the Pattern Matcher to find hypergraphs in the atomspace that are
 * similar to the query hypergraph, and returns the most similar ones.
 *
 * @param as        The atomspace that we are using
 * @param hp        The query hypergraph
 * @param rej_list  A list of atoms that we don't want any of them to exist in the results
 * @return    One or more similar hypergraphs
 */
Handle opencog::find_approximate_match(AtomSpace* as, const Handle& hp,
                                       Type rtn_type,
                                       const HandleSeq& rej_list)
{
#ifdef HAVE_GUILE
    std::cout << "!!!!! " << time(0) << "\n";
    FuzzyPatternMatchCB fpmcb(as, rtn_type, rej_list);
    std::cout << "!!!!! " << time(0) << "\n";

    HandleSeq terms;
    terms.push_back(hp);

    std::set<Handle> no_vars;

    PatternLinkPtr slp(createPatternLink(no_vars, terms));
    slp->satisfy(fpmcb);

#ifdef DEBUG
    std::cout << "\n---------- solns ----------\n";
    for (Handle h : fpmcb.solns) std::cout << h->toShortString();
#endif

    // The result_list contains a list of the grounded expressions.
    // Turn it into a true list, and return it.
    Handle gl = as->add_link(LIST_LINK, fpmcb.solns);
    std::cout << "!!!!! " << time(0) << "\n";
    return gl;
#else
    return Handle::UNDEFINED;
#endif
}

