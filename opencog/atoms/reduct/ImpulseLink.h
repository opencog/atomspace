/*
 * opencog/atoms/reduct/ImpulseLink.h
 *
 * Copyright (C) 2015,2018,2022 Linas Vepstas
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

#ifndef _OPENCOG_IMPULSE_LINK_H
#define _OPENCOG_IMPULSE_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * The ImpulseLink converts a BoolValue vector to a FloatValue vector.
 */
class ImpulseLink : public FunctionLink
{
protected:
	void init();

public:
	ImpulseLink(const HandleSeq&&, Type=IMPULSE_LINK);

	ImpulseLink(const ImpulseLink&) = delete;
	ImpulseLink& operator=(const ImpulseLink&) = delete;

	virtual ValuePtr execute(AtomSpace*, bool);
};

LINK_PTR_DECL(ImpulseLink)
#define createImpulseLink CREATE_DECL(ImpulseLink)

/** @}*/
}

#endif // _OPENCOG_IMPULSE_LINK_H
