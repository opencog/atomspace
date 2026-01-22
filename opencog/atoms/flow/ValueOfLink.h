/*
 * opencog/atoms/flow/ValueOfLink.h
 *
 * Copyright (C) 2018 Linas Vepstas
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

#ifndef _OPENCOG_VALUE_OF_LINK_H
#define _OPENCOG_VALUE_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The ValueOfLink obtains the Value on the indicated Atom (first
/// argument) at the indicated key (second argument). If that Value is
/// an executable Atom, then it is executed, and the result returned.
///
/// This design allows ValueOf to be used in constructing execution
/// chains that run automatically.  Consider, for example,
///
///    (Plus (Number 2) (ValueOf (Anchor "foo") (Predicate "bar")))
///
/// Suppose that the Value at foo-bar was a function definition; of
/// course, this would need to be run, before the Plus could be
/// performed. That function might be another ValueOf ... ad infinitum.
/// The ValueOfLink performs all of that unwrapping, delivering the
/// final result.
///
/// Use LiteralValueOf to get the unexecuted literal Value.
class ValueOfLink : public FunctionLink
{
private:
	void init(void);

protected:
	ValuePtr get_literal(AtomSpace*, bool);
	ValuePtr do_execute(AtomSpace*, bool);

public:
	ValueOfLink(const HandleSeq&&, Type=VALUE_OF_LINK);

	ValueOfLink(const ValueOfLink&) = delete;
	ValueOfLink& operator=(const ValueOfLink&) = delete;

	// Return a pointer to the value at the specified key.
	virtual ValuePtr execute(AtomSpace*, bool);

	// Experimental crazy hack...
	bool is_evaluatable(void) const { return BOOL_VALUE_OF_LINK == _type; }
	bool bevaluate(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(ValueOfLink)
#define createValueOfLink CREATE_DECL(ValueOfLink)

/** @}*/
}

#endif // _OPENCOG_VALUE_OF_LINK_H
