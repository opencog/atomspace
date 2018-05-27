/*
 * opencog/atoms/proto/RandomStream.cc
 *
 * Copyright (C) 2015, 2018 Linas Vepstas
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

#include <stdlib.h>
#include <opencog/atoms/proto/RandomStream.h>

using namespace opencog;

// ==============================================================

RandomStream::RandomStream(int len) :
	StreamValue(RANDOM_STREAM), _len(len)
{
	_value.resize(len, 0.0);
}

// ==============================================================

void RandomStream::update() const
{
	static thread_local unsigned short xsubi[3] = {0, 0, 0};
	for (int i=0; i< _len; i++)
	{
		_value[i] = erand48(xsubi);
	}
}

// ==============================================================

std::string RandomStream::to_string(const std::string& indent) const
{
	std::string rv = indent + "(" + nameserver().getTypeName(_type);
	rv += " " + std::to_string(_len);
	rv += ")\n";
	return rv;
}

// ==============================================================
