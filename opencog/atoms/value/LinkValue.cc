/*
 * opencog/atoms/value/LinkValue.cc
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

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/ValueFactory.h>

using namespace opencog;

HandleSeq LinkValue::to_handle_seq(void) const
{
	update();
	HandleSeq hs;
	for (const ValuePtr& v : _value)
	{
		if (v->is_atom())
			hs.push_back(HandleCast(v));

		// Recursively convert any value lists into atom lists
		else if (nameserver().isA(v->get_type(), LINK_VALUE))
		{
			HandleSeq hsr(LinkValueCast(v)->to_handle_seq());
			Handle h(createLink(std::move(hsr), LIST_LINK));
			hs.push_back(h);
		}
	}
	return hs;
}

HandleSet LinkValue::to_handle_set(void) const
{
	update();
	HandleSet hs;
	for (const ValuePtr& v : _value)
	{
		if (v->is_atom())
			hs.insert(HandleCast(v));

		// Recursively convert any value lists into atom lists
		else if (nameserver().isA(v->get_type(), LINK_VALUE))
		{
			HandleSeq hsr(LinkValueCast(v)->to_handle_seq());
			Handle h(createLink(std::move(hsr), LIST_LINK));
			hs.insert(h);
		}
	}
	return hs;
}

ValuePtr LinkValue::value_at_index(size_t idx) const
{
	ValuePtr vp;
	if (_value.size() > idx) vp = _value[idx];
	return createLinkValue(vp);
}

// ==============================================================

bool LinkValue::operator==(const Value& other) const
{
	// We do content-compare, and only loose type compare.
	// As long as other is a derviced type, we're good if
	// the actual values compare
printf("duuude enter linkval oper equal for %s to %s\n",
to_string().c_str(), other.to_string().c_str());
printf("duuude isa %d\n", other.is_type(LINK_VALUE));
	if (not other.is_type(LINK_VALUE)) return false;

	const LinkValue* lov = (const LinkValue*) &other;

	if (_value.size() != lov->_value.size()) 
{
printf("duudue size fail\n");
return false;
}

	// Content-compare, NOT pointer-compare!
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		if (*(_value[i]) != *(lov->_value[i])) return false;
	return true;
}

// ==============================================================

std::string LinkValue::to_string(const std::string& indent) const
{
	std::string more_indent = indent + "  "; // two spaces, same as Link
	std::string rv = indent + "(" + nameserver().getTypeName(_type) + "\n";

	// SAFE_UPDATE(rv,
(
	{
		for (const ValuePtr& v :_value)
		{
			if (v->is_atom())
				rv += v->to_short_string(more_indent) + "\n";
			else
				rv += v->to_string(more_indent) + "\n";
		}

		// Remove trailing newline before writing the last paren
		rv.pop_back();
	});
	rv += ")";
	return rv;
}

/// Similar to above, except that it does not write any newlines.
/// This avoids cogserver issues, where the newline is interpreted
/// as an end-of-messege marker by GenericShell. The SchemeShell
/// deals with pending input just fine, but the SexprShell does not:
/// its a waste of CPU-cycles to work around newlines by scanning
/// for balanced parens in strings. So ... no newlines, here.
/// Also, no indentation. Without newlines, indentation does not make
/// sense.
std::string LinkValue::to_short_string(const std::string& indent) const
{
	// update();
	std::string rv = "(" + nameserver().getTypeName(_type) + " ";
	for (const ValuePtr& v :_value)
		rv += v->to_short_string("");

	rv += ")";
	return rv;
}

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(LINK_VALUE,
                     createLinkValue, std::vector<ValuePtr>)
