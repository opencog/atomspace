/*
 * JsonSplitLink.cc
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <sstream>
#include <iomanip>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/ValueFactory.h>

#include "JsonSplitLink.h"

using namespace opencog;

JsonSplitLink::JsonSplitLink(const HandleSeq&& oset, Type t)
	: CollectionOfLink(std::move(oset), t)
{
	if (not nameserver().isA(t, JSON_SPLIT_LINK))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a JsonSplitLink, got %s", tname.c_str());
	}

	if (not _have_typespec)
	{
		_out_type = LINK_VALUE;
		_out_is_link = false;
	}
}

// ---------------------------------------------------------------

void JsonSplitLink::skip_whitespace(const std::string& str, size_t& pos)
{
	while (pos < str.length() && std::isspace(str[pos]))
		pos++;
}

// ---------------------------------------------------------------

std::string JsonSplitLink::unescape_json_string(const std::string& escaped)
{
	std::string result;
	result.reserve(escaped.length());

	for (size_t i = 0; i < escaped.length(); i++)
	{
		if (escaped[i] == '\\' && i + 1 < escaped.length())
		{
			i++;
			switch (escaped[i])
			{
				case '"':  result += '"'; break;
				case '\\': result += '\\'; break;
				case '/':  result += '/'; break;
				case 'b':  result += '\b'; break;
				case 'f':  result += '\f'; break;
				case 'n':  result += '\n'; break;
				case 'r':  result += '\r'; break;
				case 't':  result += '\t'; break;
				case 'u':
					// Handle Unicode escape sequences \uXXXX
					if (i + 4 < escaped.length())
					{
						std::string hex = escaped.substr(i + 1, 4);
						try {
							int codepoint = std::stoi(hex, nullptr, 16);
							// Simple UTF-8 encoding for BMP characters
							if (codepoint < 0x80)
							{
								result += static_cast<char>(codepoint);
							}
							else if (codepoint < 0x800)
							{
								result += static_cast<char>(0xC0 | (codepoint >> 6));
								result += static_cast<char>(0x80 | (codepoint & 0x3F));
							}
							else
							{
								result += static_cast<char>(0xE0 | (codepoint >> 12));
								result += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F));
								result += static_cast<char>(0x80 | (codepoint & 0x3F));
							}
							i += 4;
						} catch (...) {
							// Invalid hex, just include it as-is
							result += 'u';
						}
					}
					else
					{
						result += 'u';
					}
					break;
				default:
					// Unknown escape, keep the backslash and character
					result += '\\';
					result += escaped[i];
					break;
			}
		}
		else
		{
			result += escaped[i];
		}
	}

	return result;
}

// ---------------------------------------------------------------

std::string JsonSplitLink::parse_json_string(const std::string& str, size_t& pos)
{
	if (pos >= str.length() || str[pos] != '"')
		throw RuntimeException(TRACE_INFO,
			"Expected string starting with quote at position %zu", pos);

	pos++; // Skip opening quote
	std::string result;

	while (pos < str.length())
	{
		if (str[pos] == '"')
		{
			pos++; // Skip closing quote
			return unescape_json_string(result);
		}
		else if (str[pos] == '\\' && pos + 1 < str.length())
		{
			result += str[pos++];
			result += str[pos++];
		}
		else
		{
			result += str[pos++];
		}
	}

	throw RuntimeException(TRACE_INFO,
		"Unterminated string starting at position %zu", pos);
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::parse_json_number(const std::string& str, size_t& pos)
{
	size_t start = pos;

	// Handle optional minus sign
	if (pos < str.length() && str[pos] == '-')
		pos++;

	// Parse integer part
	if (pos >= str.length() || !std::isdigit(str[pos]))
		throw RuntimeException(TRACE_INFO,
			"Invalid number at position %zu", start);

	while (pos < str.length() && std::isdigit(str[pos]))
		pos++;

	// Parse optional decimal part
	if (pos < str.length() && str[pos] == '.')
	{
		pos++;
		if (pos >= str.length() || !std::isdigit(str[pos]))
			throw RuntimeException(TRACE_INFO,
				"Invalid decimal number at position %zu", start);
		while (pos < str.length() && std::isdigit(str[pos]))
			pos++;
	}

	// Parse optional exponent part
	if (pos < str.length() && (str[pos] == 'e' || str[pos] == 'E'))
	{
		pos++;
		if (pos < str.length() && (str[pos] == '+' || str[pos] == '-'))
			pos++;
		if (pos >= str.length() || !std::isdigit(str[pos]))
			throw RuntimeException(TRACE_INFO,
				"Invalid exponent at position %zu", start);
		while (pos < str.length() && std::isdigit(str[pos]))
			pos++;
	}

	std::string num_str = str.substr(start, pos - start);
	return createStringValue(num_str);
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::parse_json_literal(const std::string& str, size_t& pos)
{
	if (str.compare(pos, 4, "true") == 0)
	{
		pos += 4;
		return createStringValue("true");
	}
	else if (str.compare(pos, 5, "false") == 0)
	{
		pos += 5;
		return createStringValue("false");
	}
	else if (str.compare(pos, 4, "null") == 0)
	{
		pos += 4;
		return createStringValue("null");
	}

	throw RuntimeException(TRACE_INFO,
		"Invalid literal at position %zu", pos);
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::parse_json_array(const std::string& str, size_t& pos)
{
	if (pos >= str.length() || str[pos] != '[')
		throw RuntimeException(TRACE_INFO,
			"Expected array starting with '[' at position %zu", pos);

	pos++; // Skip '['
	ValueSeq values;

	skip_whitespace(str, pos);

	// Handle empty array
	if (pos < str.length() && str[pos] == ']')
	{
		pos++;
		return valueserver().create(_out_type, std::move(values));
	}

	while (pos < str.length())
	{
		skip_whitespace(str, pos);
		values.push_back(parse_json_value(str, pos));
		skip_whitespace(str, pos);

		if (pos >= str.length())
			throw RuntimeException(TRACE_INFO,
				"Unterminated array");

		if (str[pos] == ']')
		{
			pos++;
			return valueserver().create(_out_type, std::move(values));
		}
		else if (str[pos] == ',')
		{
			pos++;
		}
		else
		{
			throw RuntimeException(TRACE_INFO,
				"Expected ',' or ']' at position %zu", pos);
		}
	}

	throw RuntimeException(TRACE_INFO,
		"Unterminated array");
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::parse_json_object(const std::string& str, size_t& pos)
{
	if (pos >= str.length() || str[pos] != '{')
		throw RuntimeException(TRACE_INFO,
			"Expected object starting with '{' at position %zu", pos);

	pos++; // Skip '{'
	ValueSeq pairs;

	skip_whitespace(str, pos);

	// Handle empty object
	if (pos < str.length() && str[pos] == '}')
	{
		pos++;
		return valueserver().create(_out_type, std::move(pairs));
	}

	while (pos < str.length())
	{
		skip_whitespace(str, pos);

		// Parse key (must be a string)
		if (pos >= str.length() || str[pos] != '"')
			throw RuntimeException(TRACE_INFO,
				"Expected string key at position %zu", pos);

		std::string key = parse_json_string(str, pos);
		skip_whitespace(str, pos);

		// Expect colon
		if (pos >= str.length() || str[pos] != ':')
			throw RuntimeException(TRACE_INFO,
				"Expected ':' after key at position %zu", pos);

		pos++; // Skip ':'
		skip_whitespace(str, pos);

		// Parse value
		ValuePtr value = parse_json_value(str, pos);

		// Create a pair as LinkValue(StringValue(key), value)
		ValueSeq pair;
		pair.push_back(createStringValue(key));
		pair.push_back(value);
		pairs.push_back(valueserver().create(_out_type, std::move(pair)));

		skip_whitespace(str, pos);

		if (pos >= str.length())
			throw RuntimeException(TRACE_INFO,
				"Unterminated object");

		if (str[pos] == '}')
		{
			pos++;
			return valueserver().create(_out_type, std::move(pairs));
		}
		else if (str[pos] == ',')
		{
			pos++;
		}
		else
		{
			throw RuntimeException(TRACE_INFO,
				"Expected ',' or '}' at position %zu", pos);
		}
	}

	throw RuntimeException(TRACE_INFO,
		"Unterminated object");
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::parse_json_value(const std::string& str, size_t& pos)
{
	skip_whitespace(str, pos);

	if (pos >= str.length())
		throw RuntimeException(TRACE_INFO,
			"Unexpected end of JSON");

	char ch = str[pos];

	if (ch == '{')
		return parse_json_object(str, pos);
	else if (ch == '[')
		return parse_json_array(str, pos);
	else if (ch == '"')
	{
		std::string s = parse_json_string(str, pos);
		return createStringValue(s);
	}
	else if (ch == '-' || std::isdigit(ch))
		return parse_json_number(str, pos);
	else if (ch == 't' || ch == 'f' || ch == 'n')
		return parse_json_literal(str, pos);
	else
		throw RuntimeException(TRACE_INFO,
			"Unexpected character '%c' at position %zu", ch, pos);
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::rewrap_h(AtomSpace* as, const Handle& base)
{
	if (not base->is_node())
		throw RuntimeException(TRACE_INFO,
			"JsonSplitLink expects a Node!");

	const std::string& json_str = base->get_name();
	size_t pos = 0;

	try {
		return parse_json_value(json_str, pos);
	}
	catch (const std::exception& e)
	{
		throw RuntimeException(TRACE_INFO,
			"Failed to parse JSON from Node '%s': %s",
			json_str.c_str(), e.what());
	}
}

// ---------------------------------------------------------------

ValuePtr JsonSplitLink::rewrap_v(AtomSpace* as, const ValuePtr& vp)
{
	if (vp->is_type(LINK_VALUE))
	{
		LinkValuePtr lvp(LinkValueCast(vp));
		const ValueSeq& lvsq(lvp->value());
		if (lvsq.size() == 1)
			return rewrap_v(as, lvsq[0]);

		ValueSeq vsq;
		for (const ValuePtr& lvo : lvsq)
			vsq.push_back(rewrap_v(as, lvo));

		return valueserver().create(_out_type, std::move(vsq));
	}

	if (not vp->is_type(STRING_VALUE))
		throw RuntimeException(TRACE_INFO,
			"Expecting StringValue, got %s", vp->to_string().c_str());

	ValueSeq vsq;

	// StringValues hold vectors of strings. Parse each as JSON.
	StringValuePtr svp(StringValueCast(vp));
	for (const std::string& json_str : svp->value())
	{
		size_t pos = 0;
		try {
			vsq.push_back(parse_json_value(json_str, pos));
		}
		catch (const std::exception& e)
		{
			throw RuntimeException(TRACE_INFO,
				"Failed to parse JSON string '%s': %s",
				json_str.c_str(), e.what());
		}
	}

	if (vsq.size() == 1)
		return vsq[0];

	return valueserver().create(_out_type, std::move(vsq));
}

// ---------------------------------------------------------------

DEFINE_LINK_FACTORY(JsonSplitLink, JSON_SPLIT_LINK)

/* ===================== END OF FILE ===================== */
