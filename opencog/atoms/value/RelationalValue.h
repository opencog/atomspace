/*
 * opencog/atoms/value/RelationalValue.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamcis, LLC
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
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef _OPENCOG_RELATIONAL_VALUE_H
#define _OPENCOG_RELATIONAL_VALUE_H

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atoms/flow/ValueShimLink.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Base class for containers that compare Values held in the container.
 * The relation is assumed to be binary (e.g. Equal, LessThan, etc.) and
 * is sepcified in Atomese.
 */
class RelationalValue
	: public UnisetValue
{
protected:
	// Schema and its evaluation machinery
	Handle _schema;
	ValueShimLinkPtr _left_shim;
	ValueShimLinkPtr _right_shim;
	Handle _exout;
	AtomSpacePtr _scratch;

	void init_schema(void);
	bool compare(const Value& lhs, const Value& rhs) const;

	RelationalValue(Type t, const Handle& schema);
public:
	virtual ~RelationalValue();

	virtual void add(const ValuePtr&) override;
	virtual void add(ValuePtr&&) override;
	virtual std::string to_string(const std::string& indent = "") const;
};

VALUE_PTR_DECL(RelationalValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_RELATIONAL_VALUE_H
