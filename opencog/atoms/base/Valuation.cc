/*
 * opencog/atoms/base/Valuation.cc
 *
 * Copyright (C) 2015,2017 Linas Vepstas
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
#include <opencog/atoms/base/Valuation.h>

using namespace opencog;

bool Valuation::operator==(const Value& other) const
{
	if (VALUATION != other.get_type()) return false;
	Valuation* vp = (Valuation*) &other;
	if (vp->_key != _key) return false;
	if (vp->_atom != _atom) return false;
	return true;
}

// ==============================================================

std::string Valuation::to_string(const std::string& indent) const
{
	std::string rv = indent + "(Valuation\n   " + indent;
	rv += _key->to_string("") + "\n   " + indent;
	rv += _atom->to_string("") + "\n   " + indent;
	rv += _value->to_string("") + ")";
	return rv;
}

void Valuation::setValue(const ValuePtr& v)
{
	// XXX TODO -- C++ smart pointers are not atomic; we really
	// need to use a lock here, to avoid thread-races.
	_value = v;
}
