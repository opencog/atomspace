/*
 * opencog/atoms/flow/StreamValueOfLink.h
 *
 * Copyright (C) 2018, 2020 Linas Vepstas
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

#ifndef _OPENCOG_STREAM_VALUE_OF_LINK_H
#define _OPENCOG_STREAM_VALUE_OF_LINK_H

#include <opencog/atoms/flow/ValueOfLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The StreamValueOfLink returns a sample from the indicated stream.
/// It returns something of exactly the same base type as the stream,
/// effectively "freezing" the sample for all time.
///
class StreamValueOfLink : public ValueOfLink
{
public:
	StreamValueOfLink(const HandleSeq&&, Type=STREAM_VALUE_OF_LINK);

	StreamValueOfLink(const StreamValueOfLink&) = delete;
	StreamValueOfLink& operator=(const StreamValueOfLink&) = delete;

	// Return a pointer to a sample from the stream at the specified key.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(StreamValueOfLink)
#define createStreamValueOfLink CREATE_DECL(StreamValueOfLink)

/** @}*/
}

#endif // _OPENCOG_STREAM_VALUE_OF_LINK_H
