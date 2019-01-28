/*
 * opencog/atoms/execution/CondLink.cc
 *
 * Copyright (C) 2019 Kasim Ebrahim
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/core/FindUtils.h>

#include "CondLink.h"
#include "EvaluationLink.h"

using namespace opencog;

void CondLink::init(void)
{
	if (0 == _outgoing.size())
		throw SyntaxException(TRACE_INFO,
		                      "CondLink is expected to be arity greater-than 0!");

	if (1 == _outgoing.size()) {
		def_exp = _outgoing[0];
		return;
	}

	for (unsigned i = 0; i < _outgoing.size(); ++i) {
		if (i % 2 == 0) {
			if (i == _outgoing.size() - 1) {
				def_exp = _outgoing[i];
				return;
			}
			conds.push_back(_outgoing[i]);
		} else {
			exps.push_back(_outgoing[i]);
		}
	}
}

CondLink::CondLink(const HandleSeq &oset, Type t)
		: FunctionLink(oset, t)
{
	if (not nameserver().isA(t, COND_LINK)) {
		const std::string &tname = nameserver().getTypeName(t);
		throw SyntaxException(TRACE_INFO,
		                      "Expecting a CondLink, got %s", tname.c_str());
	}

	// Derived types have a different initialization sequence.
	if (COND_LINK != t) return;
	init();
}

Handle CondLink::execute(AtomSpace *scratch) const
{
	TruthValuePtr tvp;
	Instantiator inst(scratch);
	for (unsigned i = 0; i < conds.size(); ++i) {
		tvp = EvaluationLink::do_evaluate(scratch, conds[i]);
		if (tvp->get_mean() > 0.5)
			return HandleCast(inst.instantiate(exps[i], HandleMap()));
	}
	return HandleCast(inst.instantiate(def_exp, HandleMap()));
}

DEFINE_LINK_FACTORY(CondLink, COND_LINK)