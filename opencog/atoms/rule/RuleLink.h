/*
 * opencog/atoms/rule/RuleLink.h
 *
 * Copyright (C) 2015, 2022 Linas Vepstas
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
#ifndef _OPENCOG_RULE_LINK_H
#define _OPENCOG_RULE_LINK_H

#include <opencog/atoms/core/PrenexLink.h>
#include <opencog/atoms/value/QueueValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class RuleLink : public PrenexLink
{
protected:
	void init(void);

	/// The rewrite term
	HandleSeq _implicand;

	// Overwrite PatternLink::extract_variables as RuleLink has one
	// more outgoing for the rewrite rule. In addition this method
	// will initialize the rewrite term _implicand.
	void extract_variables(const HandleSeq& oset);

public:
	RuleLink(const HandleSeq&&, Type=RULE_LINK);
	RuleLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
	RuleLink(const Handle& body, const Handle& rewrite);

	RuleLink(const RuleLink&) = delete;
	RuleLink& operator=(const RuleLink&) = delete;

	virtual const HandleSeq& get_implicand(void) { return _implicand; }

	virtual bool is_executable() const { return false; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(RuleLink)
#define createRuleLink CREATE_DECL(RuleLink)

/** @}*/
}

#endif // _OPENCOG_RULE_LINK_H
