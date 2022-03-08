/*
 * opencog/atoms/flow/SetTVLink.h
 *
 * Copyright (C) 2020 Linas Vepstas
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

#ifndef _OPENCOG_SET_TV_LINK_H
#define _OPENCOG_SET_TV_LINK_H

#include <opencog/atoms/flow/SetValueLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The SetTVLink sets the TruthValue on the indicated atom
/// (first argument) to the TruthValue obtained by evaluating
/// the second argument.
///
class SetTVLink : public SetValueLink
{
public:
	SetTVLink(const HandleSeq&&, Type=SET_TV_LINK);

	SetTVLink(const SetTVLink&) = delete;
	SetTVLink& operator=(const SetTVLink&) = delete;

	// Return a pointer to the TV that was set.
	virtual bool is_evaluatable() const { return true; }
	virtual TruthValuePtr evaluate(AtomSpace*, bool);
	virtual ValuePtr execute(AtomSpace* as, bool silent) {
		return ValueCast(evaluate(as, silent));
	}

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(SetTVLink)
#define createSetTVLink std::make_shared<SetTVLink>

/** @}*/
}

#endif // _OPENCOG_SET_TV_LINK_H
