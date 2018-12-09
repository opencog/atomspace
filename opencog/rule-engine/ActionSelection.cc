/*
 * ActionSelection.cc
 *
 * Copyright (C) 2017 OpenCog Foundation
 *
 * Authors: Nil Geisweiller
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

#include "ActionSelection.h"

#include <boost/range/adaptor/map.hpp>

#include <opencog/util/Logger.h>
#include <opencog/util/oc_assert.h>

namespace opencog {

ActionSelection::ActionSelection(const HandleTVMap& a2tv)
	: action2tv(a2tv)
	, _tvs(boost::adaptors::values(a2tv).begin(),
	       boost::adaptors::values(a2tv).end())
	, _tsmp(_tvs) {}

HandleCounter ActionSelection::distribution()
{
	HandleCounter action2prob;
	std::vector<double> probs = _tsmp.distribution();
	size_t i = 0;
	for (const auto& atv : action2tv) {
		action2prob[atv.first] = probs[i];
		i++;
	}
	return action2prob;
}

std::string ActionSelection::to_string(const std::string& indent) const
{
	std::stringstream ss;
	ss << indent << "action2tv:" << std::endl
	   << oc_to_string(action2tv, indent + OC_TO_STRING_INDENT)
	   << indent << "thompson sampling:" << std::endl
	   << oc_to_string(_tsmp, indent + OC_TO_STRING_INDENT);
	return ss.str();
}

std::string oc_to_string(const ActionSelection& asel, const std::string& indent)
{
	return asel.to_string();
}

std::string oc_to_string(const HandleTVMap& h2tv, const std::string& indent)
{
	std::stringstream ss;
	ss << indent << "size = " << h2tv.size() << std::endl;
	int i = 0;
	for (const auto& htv : h2tv) {
		ss << indent << "atom[" << i << "]:" << std::endl
		   << oc_to_string(htv.first, indent + OC_TO_STRING_INDENT);
		ss << indent << "tv[" << i << "]:"
		   << oc_to_string(htv.second) << std::endl;
	}
	return ss.str();
}

} // ~namespace opencog
