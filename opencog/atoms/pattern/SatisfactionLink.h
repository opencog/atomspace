/*
 * opencog/atoms/pattern/SatisfactionLink.h
 *
 * Copyright (C) 2015, 2016 Linas Vepstas
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
#ifndef _OPENCOG_SATISFACTION_LINK_H
#define _OPENCOG_SATISFACTION_LINK_H

#include <opencog/atoms/pattern/PatternLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class SatisfactionLink : public PatternLink
{
protected:
	void init(void);
public:
	SatisfactionLink(const HandleSeq&&, Type=SATISFACTION_LINK);

	SatisfactionLink(const SatisfactionLink&) = delete;
	SatisfactionLink& operator=(const SatisfactionLink&) = delete;

	virtual bool is_evaluatable() const { return true; }
	virtual bool bevaluate(AtomSpace*, bool);

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace* as, bool silent) {
		return ValueCast(evaluate(as, silent)); }


	static Handle factory(const Handle&);
};

LINK_PTR_DECL(SatisfactionLink)
#define createSatisfactionLink CREATE_DECL(SatisfactionLink)

/** @}*/
}

#endif // _OPENCOG_SATISFACTION_LINK_H
