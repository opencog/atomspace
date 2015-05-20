/*
 * ForwardChainPatternMatchCB.cc
 *
 * Copyright (C) 2014 Misgana Bayetta
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Sept 2014
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

#include <opencog/rule-engine/forwardchainer/ForwardChainerPMCB.h>

using namespace opencog;

ForwardChainerPMCB::ForwardChainerPMCB(AtomSpace * as) :
		Implicator(as), InitiateSearchCB(as), DefaultPatternMatchCB(as), AttentionalFocusCB(
				as), _as(as) {
	_fcmem = nullptr;
}

ForwardChainerPMCB::~ForwardChainerPMCB() {
}

bool ForwardChainerPMCB::node_match(const Handle& node1,
		const Handle& node2) {
	//constrain search within premise list
	bool accept =
			_fcmem->is_search_in_af() ?
					AttentionalFocusCB::node_match(node1, node2) :
					DefaultPatternMatchCB::node_match(node1, node2);

    return ret;

}

bool ForwardChainerPMCB::link_match(const LinkPtr& lpat,
		const LinkPtr& lsoln) {
	//constrain search within premise list
	bool accpet =
			_fcmem->is_search_in_af() ?
					AttentionalFocusCB::link_match(lpat, lsoln) :
					DefaultPatternMatchCB::link_match(lpat, lsoln);

    return ret;
}

bool ForwardChainerPMCB::grounding(
		const std::map<Handle, Handle> &var_soln,
		const std::map<Handle, Handle> &pred_soln) {
	Handle source = _fcmem->get_cur_source();

	for (auto it = var_soln.begin(); it != var_soln.end(); ++it) {
		//Accept solution if any one of the variables ground to the source
		if (it->second == source) {
			Handle h = inst.instantiate(implicand, var_soln);
			if (Handle::UNDEFINED != h) {
				result_list.push_back(h);
			}
			return true;
		}
	}

	return false;
}

void ForwardChainerPMCB::set_fcmem(FCMemory *fcmem) {
	_fcmem = fcmem;
}
HandleSeq ForwardChainerPMCB::get_products() {
	auto product = result_list;
	result_list.clear();
	return product;
}
