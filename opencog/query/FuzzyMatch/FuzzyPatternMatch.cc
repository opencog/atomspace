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

#include <opencog/atoms/bind/SatisfactionLink.h>

#include "FuzzyPatternMatchCB.h"
#include "FuzzyPatternMatch.h"

using namespace opencog;

//#define DEBUG

/**
 * Implement the "cog-fuzzy-match" scheme primitive.
 * It uses the Pattern Matcher to find hypergraphs in the atomspace that are
 * similar to the query hypergraph, and returns the most similar ones.
 *
 * @param hp  The query hypergraph
 * @return    One or more similar hypergraphs
 */
Handle opencog::find_approximate_match(AtomSpace* as, const Handle& hp)
{
#ifdef HAVE_GUILE
    FuzzyPatternMatchCB fpmcb(as);

    HandleSeq terms;
    terms.push_back(hp);

    std::set<Handle> no_vars;

    SatisfactionLinkPtr slp(createSatisfactionLink(no_vars, terms));
    slp->satisfy(fpmcb);

#ifdef DEBUG
    std::cout << "\n---------- solns ----------\n";
    for (Handle h : fpmcb.solns) std::cout << h->toShortString();
#endif

    // The result_list contains a list of the grounded expressions.
    // Turn it into a true list, and return it.
    Handle gl = as->addLink(LIST_LINK, fpmcb.solns);
    return gl;
#else
    return Handle::UNDEFINED;
#endif
}

