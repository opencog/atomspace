/*
 * opencog/atoms/value/ContainerValue.cc
 *
 * Copyright (C) 2020, 2025 Linas Vepstas
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

#include <opencog/atoms/value/ContainerValue.h>

#include <sstream>

using namespace opencog;

// ==============================================================

std::string ContainerValue::to_string(const std::string& indent) const
{
	// The default printer for ContainerValue is LinkValue ...
	// with only one small problem: containers can block under certain
	// conditions.  This provides a non-blocking printer.
	//
	// The BLOCKING_SIG containers will hang if the container is open,
	// and the STREAMING_SIG containers will hang if the container is
	// empty.
	//
	// XXX FIXME, below is correct only for BLOCKING_SIG ...
	if (is_closed()) return LinkValue::to_string(indent);

	return indent + "(" + nameserver().getTypeName(get_type()) +
	   ") ;; open for writing";
}

// ==============================================================

bool ContainerValue::operator==(const Value& other) const
{
	if (this == &other) return true;

	if (not is_closed()) return false;
	if (other.is_type(CONTAINER_VALUE) and
	    not ((const ContainerValue*) &other)->is_closed()) return false;

	return LinkValue::operator==(other);
}

// ==============================================================
