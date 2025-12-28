/*
 * opencog/atoms/execution/EvaluatableLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, LLC
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

#ifndef _OPENCOG_EVALUATABLE_LINK_H
#define _OPENCOG_EVALUATABLE_LINK_H

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/BoolValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The EvaluatableLink is the base class for all links that can be
/// evaluated, resulting in a truth value.
///
class EvaluatableLink : public Link
{
	bool _unordered;
public:
	EvaluatableLink(const HandleSeq&&, Type=EVALUATABLE_LINK);

	EvaluatableLink(const EvaluatableLink&) = delete;
	EvaluatableLink& operator=(const EvaluatableLink&) = delete;

	virtual bool is_evaluatable() const { return true; }
	virtual bool is_executable() const { return true; }

	virtual bool bevaluate(AtomSpace*, bool silent=false);
	virtual ValuePtr execute(AtomSpace* as, bool silent=false) {
		return ValueCast(createBoolValue(bevaluate(as, silent)));
	}

	virtual bool is_unordered_link() const { return _unordered; }

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(EvaluatableLink)
#define createEvaluatableLink CREATE_DECL(EvaluatableLink)

/** @}*/
}

#endif // _OPENCOG_EVALUATABLE_LINK_H
