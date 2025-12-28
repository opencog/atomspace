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
	if (other.is_type(CONTAINER_VALUE))
	{
		const ContainerValue* cvp = (const ContainerValue*) &other;
		if (not cvp->is_closed()) return false;
		// Why call update()? See comment below.
		cvp->update();
	}

	// The LinkValue::operator==() will fail unless data has been moved
	// from the container to the _value. The update() will do this
	// movement. Its safe to do now, too, since the container is closed.
	update();
	return LinkValue::operator==(other);
}

// ==============================================================

/// Provide override ordering for non-closed containers.
/// As currently implemented, this is not a stable ordering;
/// it will change when the container closes. This seems like
/// maybe a bad idea; on the other hand, we do want to get the
/// contentional lexicographic compare when they are closed.
/// So this is a confused mashup. Punt on a final decision to
/// some future date, when this becomes more clear.
///
/// Why is this a concern? Well, UnisetValues can contain *other*
/// (open) UnisetValues, and if we don't get this right, they get
/// deduplicated by accident.
bool ContainerValue::operator<(const Value& other) const
{
	if (this == &other) return false;

	if (not is_closed()) return this < &other;

	if (other.is_type(CONTAINER_VALUE) and
	    not ((const ContainerValue*) &other)->is_closed())
		return this < &other;

	return LinkValue::operator<(other);
}

// ==============================================================
