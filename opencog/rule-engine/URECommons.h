/*
 * URECommons.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Oct 2014
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
#ifndef _OPENCOG_URECOMMONS_H_
#define _OPENCOG_URECOMMONS_H_

#include <opencog/util/dorepeat.h>
#include <opencog/util/random.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/proto/types.h>

namespace opencog {

using namespace std;

/**
 * Reusable functions in the PLN module
 */
class URECommons {
	AtomSpace& _as;

public:
	const float FITNESS_PARAM = 0.9;
	URECommons(AtomSpace& as);

	/**
	 * Randomly pick about half of the elements, and amongst those
	 * return the fittest (higher is better). If tfitness_map is
	 * empty, then exception is thrown.
	 *
	 * TODO: generalize and move this method to
	 * opencog/util/selection.h
	 */
	template<class Type>
	Type tournament_select(const map<Type, double>& tfitnes_map) {
		// Nothing to select, return the nullptr rule
		if (tfitnes_map.empty())
			 throw RuntimeException(TRACE_INFO,
			                        "[URECommons] Empty fitness map provided.");

		// Something to select, randomly pick (without replacement)
		// about half of the rules and return the best.
		//
		// TODO change the way pick_size is calculated.
		size_t pick_size = std::max(static_cast<size_t>(1),
		                            tfitnes_map.size() / 2);
		multimap<double, Type> winners;
		dorepeat(pick_size)
		{
			auto el = rand_element(tfitnes_map);
			winners.insert( { el.second, el.first });
		}
		return winners.rbegin()->second;
	}

	/**
	 * Calculates fitness values in source_list_atom_space (or
	 * target_list_atom_space for the BC) using the formula
	 *
	 * F = s^x * c^(2-x)
	 *
	 * where s is strength, c is confidence and x is some fixed value
	 *
	 * @param h - a handle
	 * @return a fitness value
	 */
	double tv_fitness(const Handle& h) const;
};

} // ~namespace opencog

#endif /* _OPENCOG_URECOMMONS_H_ */
