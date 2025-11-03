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

// ==============================================================

bool LinkValue::operator==(const Value& other) const
{
	// We do content-compare, and only loose type compare.
	// As long as other is a derviced type, we're good if
	// the actual values compare
	if (not other.is_type(LINK_VALUE)) return false;

	const LinkValue* lov = (const LinkValue*) &other;

	if (_value.size() != lov->_value.size()) return false;

	// Content-compare, NOT pointer-compare!
	size_t len = _value.size();
	for (size_t i=0; i<len; i++)
		if (*(_value[i]) != *(lov->_value[i])) return false;
	return true;
}

bool LinkValue::operator<(const Value& other) const
{
	// Compare by type name.
	if (_type != other.get_type())
		return nameserver().getTypeName(_type) < nameserver().getTypeName(other.get_type());

	// Compare by vector length.
	const LinkValue* lov = (const LinkValue*) &other;
	if (_value.size() != lov->_value.size())
		return _value.size() < lov->_value.size();

	// Compare individual values lexicographically.
	// This works because std::less<ValuePtr> is specialized to compare content.
	return _value < lov->_value;
}

// ==============================================================

std::string LinkValue::to_string(const std::string& indent, Type t) const
{
	std::string more_indent = indent + "  "; // two spaces, same as Link
	std::string rv = indent + "(" + nameserver().getTypeName(t) + "\n";

	SAFE_UPDATE(rv,
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
	update();
	std::string rv = "(" + nameserver().getTypeName(_type) + " ";
	for (const ValuePtr& v :_value)
	{
		// Well .. this is kind of insane and mildly unpleasant, but...
		// It can happen that Values get created that have null pointers
		// in them. Yes, that's ugly and bad. We should not throw here,
		// because this is long after the creation of the Value; we
		// are here because atomspace-storage is trying to serialize us.
		// We hae two remaining choices: do nothing and print nothing,
		// or print (VoidValue) to indicate end-of-stream, end-of-file,
		// no-data, whatever. See that just right now, this is a better
		// idea than printing nothing at all. We'll see. The correct
		// long-term fix is to avoid null pointers in ValueSeq's.
		if (v)
			rv += v->to_short_string("");
		else
			rv += "(VoidValue)";
	}

	rv += ")";
	return rv;
}

// Adds factory when library is loaded.
DEFINE_VALUE_FACTORY(LINK_VALUE, createLinkValue, ValueSeq&&)
DEFINE_VALUE_FACTORY(LINK_VALUE, createLinkValue, const HandleSeq&)
