/*
 * opencog/atoms/flow/PromiseLink.h
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

#ifndef _OPENCOG_PROMISE_LINK_H
#define _OPENCOG_PROMISE_LINK_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The PromiseLink returns the provided atom wrapped with a future.
/// The three future base clases are StreamValue, LinkStream and
/// FutureTruthValue.
///
class PromiseLink : public Link
{
private:
	void init(void);
	Type _type;

public:
	PromiseLink(const HandleSeq&&, Type=PROMISE_LINK);
	PromiseLink(const Handle&);
	PromiseLink(const Handle&, const Handle&);

	PromiseLink(const PromiseLink&) = delete;
	PromiseLink& operator=(const PromiseLink&) = delete;

	// Return a future
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(PromiseLink)
#define createPromiseLink CREATE_DECL(PromiseLink)

/** @}*/
}

#endif // _OPENCOG_PROMISE_LINK_H
