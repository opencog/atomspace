/*
 * opencog/atoms/flow/JsonSplitLink.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
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

#ifndef _OPENCOG_JSON_SPLIT_LINK_H
#define _OPENCOG_JSON_SPLIT_LINK_H

#include <opencog/atoms/flow/CollectionOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The JsonSplitLink parses JSON strings and converts them to Atomese
/// key-value pairs. JSON objects (curly braces) become LinkValue wrapping
/// pairs of StringValue. JSON arrays (square brackets) become LinkValue
/// holding sequences. Handles proper JSON escaping/unescaping.
///
class JsonSplitLink : public CollectionOfLink
{
protected:
	virtual ValuePtr rewrap_h(AtomSpace*, const Handle&);
	virtual ValuePtr rewrap_v(AtomSpace*, const ValuePtr&);

	ValuePtr parse_json_value(const std::string&, size_t&);
	ValuePtr parse_json_object(const std::string&, size_t&);
	ValuePtr parse_json_array(const std::string&, size_t&);
	std::string parse_json_string(const std::string&, size_t&);
	ValuePtr parse_json_number(const std::string&, size_t&);
	ValuePtr parse_json_literal(const std::string&, size_t&);

	void skip_whitespace(const std::string&, size_t&);
	std::string unescape_json_string(const std::string&);

public:
	JsonSplitLink(const HandleSeq&&, Type = JSON_SPLIT_LINK);
	JsonSplitLink(const JsonSplitLink&) = delete;
	JsonSplitLink& operator=(const JsonSplitLink&) = delete;

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(JsonSplitLink)
#define createJsonSplitLink CREATE_DECL(JsonSplitLink)

/** @}*/
}

#endif // _OPENCOG_JSON_SPLIT_LINK_H
