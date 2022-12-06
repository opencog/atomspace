/*
 * opencog/atoms/rule/ConclusionOfLink.h
 *
 * Copyright (C) 2018, 2022 Linas Vepstas
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

#ifndef _OPENCOG_CONCLUSION_OF_LINK_H
#define _OPENCOG_CONCLUSION_OF_LINK_H

#include <opencog/atoms/rule/VardeclOfLink.h>
#include <opencog/atoms/rule/RuleLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ConclusionOfLink returns the n'th premise on the given RuleLink
class ConclusionOfLink : public VardeclOfLink
{
private:
	void init(void);

protected:
	RuleLinkPtr _rule;
	Handle _conclusion;

public:
	ConclusionOfLink(const HandleSeq&&, Type=CONCLUSION_OF_LINK);

	ConclusionOfLink(const ConclusionOfLink&) = delete;
	ConclusionOfLink& operator=(const ConclusionOfLink&) = delete;

	// Return the n'th conclusion of the rule.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ConclusionOfLink)
#define createConclusionOfLink CREATE_DECL(ConclusionOfLink)

/** @}*/
}

#endif // _OPENCOG_CONCLUSION_OF_LINK_H
