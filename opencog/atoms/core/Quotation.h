/*
 * opencog/atoms/core/Quotation.h
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 *
 * Written by Nil Geisweiller
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

#ifndef _OPENCOG_QUOTATION_H
#define _OPENCOG_QUOTATION_H

#include <string>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/atom_types.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Quotation data and methods. See http://wiki.opencog.org/w/QuoteLink.
 */
class Quotation
{
	int _quotation_level;
	bool _local_quote;

public:
	explicit Quotation(int ql=0, bool lq=false);

	int level() const;
	bool is_locally_quoted() const;
	bool is_quoted() const;
	bool is_unquoted() const;

	/**
	 * Return true iff the type is QUOTE_LINK, UNQUOTE_LINK or
	 * LOCAL_QUOTE_LINK.
	 */
	static bool is_quotation_type(Type t);

	/**
	 * Check whether the current atom would be consumed as quotation
	 * operator or not. More specifically:
	 *
	 * 1. An unquoted QuoteLink would be consumed, turning the
	 * quotation state from unquoted to quoted.
	 *
	 * 2. An UnquoteLink with quotation level of 1 would be consumed,
	 * turning the quotation state from quoted to unquoted.
	 *
	 * 3. An unquoted LocalQuoteLink would be consumed, turning
	 * temporarily the quotation state from unquoted to quoted.
	 *
	 * Remark: you must call this function before calling update,
	 * otherwise the result will not be correct on the current atom.
	 */
	bool consumable(Type t) const;

	/**
	 * Increment, decrement or change local quote given the type of
	 * the current atom before handing the quotation to the outgoing.
	 */
	void update(Type t);

	/**
	 * Comparison
	 */
	bool operator<(const Quotation& quotation) const;
	bool operator==(const Quotation& quotation) const;

	std::string to_string(const std::string& indent) const;
};

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const Quotation& quotation,
                         const std::string& indent=empty_string);
	
/** @}*/
} // namespace opencog

#endif // _OPENCOG_QUOTATION_H
