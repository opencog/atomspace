/*
 * opencog/atomspace/ImportanceIndex.cc
 *
 * Copyright (C) 2008-2011 OpenCog Foundation
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
 */

#include <algorithm>

#include <opencog/util/functional.h>

#include <opencog/atomspace/ImportanceIndex.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atomspace/AtomTable.h>

#include <boost/range/adaptor/reversed.hpp>

using namespace opencog;

/**
 * This formula is used to calculate the GROUP_NUM given the GROUP_SIZE
 * 32768 = (sum 2^b , b = 0 to (GROUP_NUM-1)) * GROUP_SIZE + GROUP_SIZE
 * for a GROUP_SIZE of 8 we get:
 * 32768 = (sum 2^b , b = 0 to 11) * 8 + 8
 * This means we have 12 groups with 8 bins each
 * The range of each groups bins is double the previous (2^b) and starts at 1
 * We have to add 8 since [2^c - 1 = sum 2^b , b = 0 to (c-1)] and we have 8
 * such groups
 */
#define GROUP_SIZE 8
#define GROUP_NUM 12
#define IMPORTANCE_INDEX_SIZE (GROUP_NUM*GROUP_SIZE)+GROUP_NUM //104

ImportanceIndex::ImportanceIndex(void)
{
	resize(IMPORTANCE_INDEX_SIZE);
}

unsigned int ImportanceIndex::importanceBin(short importance)
{
    if (importance < 2*GROUP_SIZE)
        return importance;

    short imp = std::ceil((importance - GROUP_SIZE) / GROUP_SIZE);

    int sum = 0;
    int i;
    for (i = 0; i <= GROUP_NUM; i++)
    {
        if (sum >= imp)
            break;
        sum = sum + std::pow(2,i);
    }

    int ad = GROUP_SIZE - std::ceil(importance / std::pow(2,(i-1)));

    unsigned int bin = ((i * GROUP_SIZE) - ad);
    assert(bin <= IMPORTANCE_INDEX_SIZE);
    return bin;
}

void ImportanceIndex::updateImportance(Atom* atom, int bin)
{
	int newbin = importanceBin(atom->getAttentionValue()->getSTI());
	if (bin == newbin) return;

	remove(bin, atom);
	insert(newbin, atom);
}

void ImportanceIndex::insertAtom(Atom* atom)
{
	int sti = atom->getAttentionValue()->getSTI();
	int bin = importanceBin(sti);
	insert(bin, atom);
}

void ImportanceIndex::removeAtom(Atom* atom)
{
	int sti = atom->getAttentionValue()->getSTI();
	int bin = importanceBin(sti);
	remove(bin, atom);
}

UnorderedHandleSet ImportanceIndex::getHandleSet(
        AttentionValue::sti_t lowerBound,
        AttentionValue::sti_t upperBound) const
{
	AtomSet set;

	// The indexes for the lower bound and upper bound lists is returned.
	int lowerBin = importanceBin(lowerBound);
	int upperBin = importanceBin(upperBound);

	// Build a list of atoms whose importance is equal to the lower bound.
	// For the lower bound and upper bound index, the list is filtered,
	// because there may be atoms that have the same importanceIndex
	// and whose importance is lower than lowerBound or bigger than
	// upperBound.
	const AtomSet &sl = idx[lowerBin];
	std::copy_if(sl.begin(), sl.end(), inserter(set),
		[&](Atom* atom)->bool {
			AttentionValue::sti_t sti =
				atom->getAttentionValue()->getSTI();
			return (lowerBound <= sti and sti <= upperBound);
		});

	// If both lower and upper bounds are in the same bin,
	// Then we are done.
	if (lowerBin == upperBin) {
		UnorderedHandleSet ret;
		std::transform(set.begin(), set.end(), inserter(ret),
		               [](Atom* atom)->Handle { return atom->getHandle(); });
		return ret;
	}

	// For every index within lowerBound and upperBound,
	// add to the list.
	while (++lowerBin < upperBin) {
		const AtomSet &ss = idx[lowerBin];
		set.insert(ss.begin(), ss.end());
	}

	// The two lists are concatenated.
	const AtomSet &uset = idx[upperBin];
	std::copy_if(uset.begin(), uset.end(), inserter(set),
		[&](Atom* atom)->bool {
			AttentionValue::sti_t sti = atom->getAttentionValue()->getSTI();
			return (lowerBound <= sti and sti <= upperBound);
		});

	UnorderedHandleSet ret;
	std::transform(set.begin(), set.end(), inserter(ret),
	               [](Atom* atom)->Handle { return atom->getHandle(); });
	return ret;
}

UnorderedHandleSet ImportanceIndex::getMaxBinContents()
{
    UnorderedHandleSet ret;
    for (AtomSet s : boost::adaptors::reverse(idx))
    {
        if (s.size() > 0)
        {
	        std::transform(s.begin(), s.end(), inserter(ret),
	               [](Atom* atom)->Handle { return atom->getHandle(); });
	        return ret;
        }
    }

    return ret;
}

UnorderedHandleSet ImportanceIndex::getMinBinContents()
{
    UnorderedHandleSet ret;
    for (AtomSet s : idx)
    {
        if (s.size() > 0)
        {
	        std::transform(s.begin(), s.end(), inserter(ret),
	               [](Atom* atom)->Handle { return atom->getHandle(); });
	        return ret;
        }
    }

    return ret;
}
