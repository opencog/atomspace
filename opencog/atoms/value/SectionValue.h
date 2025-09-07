/*
 * opencog/atoms/value/SectionValue.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_SECTION_VALUE_H
#define _OPENCOG_SECTION_VALUE_H

#include <opencog/atoms/value/LinkValue.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * SectionValue is like SectionLink, for for Values.
 * Should be arity two: a name/id and a connector seq.
 * Experimental; seems to make sense and is desirable to have.
 * Used by atomese-simd.
 */
class SectionValue
	: public LinkValue
{
protected:
	SectionValue(Type t) : LinkValue(t) {}
public:
	SectionValue(const ValueSeq& vlist);
	SectionValue(ValueSeq&& vlist);
	virtual ~SectionValue() {}
};

VALUE_PTR_DECL(SectionValue);
CREATE_VALUE_DECL(SectionValue);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_SECTION_VALUE_H
