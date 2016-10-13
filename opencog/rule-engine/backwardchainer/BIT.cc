/*
 * BIT.cc
 *
 * Author: William Ma <https://github.com/williampma>
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#include <opencog/util/random.h>

#include <opencog/atomutils/Neighbors.h>
#include "BIT.h"
#include "BCLogger.h"

namespace opencog {

BITNode::BITNode(const Handle& bd, const Handle& vd, const BITFitness& fit)
	: body(bd), vardecl(vd), fitness(fit) {}

std::string	BITNode::to_string() const
{
	stringstream ss;
	ss << "body:" << std::endl << oc_to_string(body)
	   << "vardecl:" << std::endl << oc_to_string(vardecl)
	   << "rules:" << std::endl << oc_to_string(rules);
	return ss.str();
}

std::string oc_to_string(const BITNode& bitnode)
{
	return bitnode.to_string();
}

std::string oc_to_string(const BITNodePtr& bitnode_ptr)
{
	return bitnode_ptr->to_string();
}

} // ~namespace opencog
