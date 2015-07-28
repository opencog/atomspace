/*
 * FuzzyPatternMatchCB.h
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

#ifndef FUZZYPATTERNMATCHCB_H
#define FUZZYPATTERNMATCHCB_H

#include <opencog/query/DefaultPatternMatchCB.h>

namespace opencog
{
    class FuzzyPatternMatchCB :
		public DefaultPatternMatchCB
    {
        public:
            // The solutions
            HandleSeq solns;

            FuzzyPatternMatchCB(AtomSpace* as, const HandleSeq& rl);

            virtual bool initiate_search(PatternMatchEngine* pme);

            virtual void set_pattern(const Variables& vars,
                                     const Pattern& pat)
            {
                DefaultPatternMatchCB::set_pattern(vars, pat);
                _pattern = &pat;
            }

            virtual bool fuzzy_match(const Handle& h1, const Handle& h2)
            {
                return true;
            }

            virtual bool link_match(const LinkPtr& pLink, const LinkPtr& gLink);

            virtual bool node_match(const Handle& pNode, const Handle& gNode)
            {
                return true;
            }

            virtual bool grounding(const std::map<Handle, Handle>& var_soln,
                                   const std::map<Handle, Handle>& term_soln)
            {
                return true;
            }

        private:
            const Pattern* _pattern = NULL;

            Handle clause;

            HandleSeq reject_list;

            struct Starter
            {
                UUID uuid;
                Handle handle;
                Handle term;
                size_t width;
                size_t depth;
            };

            // Total number of links in the atomspace
            size_t num_links;

            // How many nodes are there in the pattern
            size_t pat_size = 0;

            // Potential starters that can be used to initiate the search
            std::vector<Starter> potential_starters;

            // Links that have previously been compared
            std::vector<UUID> prev_compared;

            // Stores the incoming set sizes of nodes
            std::unordered_map<Handle, size_t> in_set_size;

            // The maximum similarity of all the potential solutions we found
            double max_similarity = -std::numeric_limits<double>::max();

            void find_starters(const Handle& hg, const size_t& depth,
                               const size_t& clause_idx, const Handle& term,
                               std::vector<Starter>& rtn);

            void check_if_accept(const Handle& ph, const Handle& gh);
    };
}

#endif // FUZZYPATTERNMATCHCB_H

