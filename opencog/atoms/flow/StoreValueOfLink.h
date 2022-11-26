/*
 * opencog/atoms/flow/StoreValueOfLink.h
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

#ifndef _OPENCOG_STORE_VALUE_OF_LINK_H
#define _OPENCOG_STORE_VALUE_OF_LINK_H

#include <opencog/atoms/flow/ValueOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The StoreValueOfLink gets a Value from and open StorageNode,
/// sets it at the key, and then returns that Value.
///
class StoreValueOfLink : public ValueOfLink
{
private:
	void init(void);

public:
	StoreValueOfLink(const HandleSeq&&, Type = STORE_VALUE_OF_LINK);
	StoreValueOfLink(const StoreValueOfLink&) = delete;
	StoreValueOfLink& operator=(const StoreValueOfLink&) = delete;

	// Return a pointer to the Value being updated.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(StoreValueOfLink)
#define createStoreValueOfLink CREATE_DECL(StoreValueOfLink)

/** @}*/
}

#endif // _OPENCOG_STORE_VALUE_OF_LINK_H
