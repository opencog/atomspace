/*
 * DualLink.cc
 *
 * Copyright (C) 2014-2016 Linas Vepstas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/UnorderedLink.h>
#include <opencog/query/Recognizer.h>

#include "DualLink.h"

using namespace opencog;

void DualLink::init(void)
{
	Type t = get_type();
	if (not nameserver().isA(t, DUAL_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a DualLink, got %s", tname.c_str());
	}

	_pat.redex_name = "anonymous DualLink";

	_num_virts = 1;
	_num_comps = 1;

	// At this time, we don't support DualLinks with variables.
	// We could, be we don't. Thus, the initialization here is
	// extremely simple.
	if (1 < _outgoing.size())
		throw InvalidParamException(TRACE_INFO,
			"DualLinks with variables are currently not supported.\n");

	// ScopeLink::extract_variables(_outgoing);
	_body = _outgoing[0];

	PatternTermPtr root_term(make_term_tree(_body));
	_pat.pmandatory.emplace_back(root_term);
	_fixed.emplace_back(root_term);

	_pat.body = _body;

	// Well, there won't be any variables, but we still need
	// to have an empty-set-per-clause to not trigger asserts
	// in the PatternMatcherEngine. We could make the engine
	// smarter, at the cost of slowing down others. So we
	// won't do that. We'll just hack it up here.
	clauses_get_variables(_pat.pmandatory);
}

DualLink::DualLink(const HandleSeq&& hseq, Type t)
	: PatternLink(std::move(hseq), t)
{
	init();
}

ValuePtr DualLink::execute(AtomSpace* as, bool silent)
{
	if (nullptr == as) as = _atom_space;
	Recognizer reco(as);
	reco.satisfy(PatternLinkCast(get_handle()));
	return as->add_atom(createUnorderedLink(reco._rules, SET_LINK));
}

DEFINE_LINK_FACTORY(DualLink, DUAL_LINK)

/* ===================== END OF FILE ===================== */
