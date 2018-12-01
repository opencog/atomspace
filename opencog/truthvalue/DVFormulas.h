/*
 * opencog/truthvalue/DVFormulas.h
 *
 * Copyright (C) 2018 SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#ifndef _OPENCOG_DV_FORMULAS_H
#define _OPENCOG_DV_FORMULAS_H

#include <memory>
#include <string>
#include <vector>
#include <limits>

#include <opencog/util/exceptions.h>
#include <opencog/atoms/proto/ProtoAtom.h>
#include <opencog/atoms/base/Handle.h>

#include <opencog/truthvalue/ConditionalDV.h>
#include <opencog/truthvalue/DistributionalValue.h>

/** \addtogroup grp_atomspace
 *	@{
 */

namespace opencog
{

class DVFormulas
{

public:
	static DVec get_key_min(DVKey);
	static DVec get_key_max(DVKey);
	//static double interval_dist(DVKey,DVKey);

	//was divide
	static ConditionalDVPtr joint_to_cdv(DistributionalValuePtr
		         						,DistributionalValuePtr
										,int);

	DistributionalValuePtr sum_joint(DistributionalValuePtr,int) const;
	DVCounter part_joint(DistributionalValuePtr,int) const;

	typedef std::pair<DVKey,double> Elem;
	typedef std::function<bool(Elem,Elem)> Comparator;

	static bool compare(DVec,DVec);
	static bool comperator(Elem,Elem);

	static DistributionalValuePtr conjuction(DistributionalValuePtr
			                                ,DistributionalValuePtr);
	static DistributionalValuePtr disjuction(DistributionalValuePtr
			  	                            ,DistributionalValuePtr);

	//double middle_of_interval(DVKey) const;
};

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
