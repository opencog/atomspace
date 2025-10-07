/*
 * opencog/atoms/flow/MessagesOfLink.h
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

#ifndef _OPENCOG_MESSAGES_OF_LINK_H
#define _OPENCOG_MESSAGES_OF_LINK_H

#include <opencog/atoms/core/FunctionLink.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// The MessagesOfLink returns a LinkValue holding all of the messages
/// that are supported by the atoms in the outgoing set.
///
/// For example,
///
///     MessagesOfLink
///         Object "foo"
///         Object "bar"
///
/// will return
///
///     (LinkValue (Predicate "msg1") (Predicate "msg2") ...)
///
/// containing all messages from both atoms, merged into one set.
///
class MessagesOfLink : public FunctionLink
{
public:
	MessagesOfLink(const HandleSeq&&, Type = MESSAGES_OF_LINK);
	MessagesOfLink(const MessagesOfLink&) = delete;
	MessagesOfLink& operator=(const MessagesOfLink&) = delete;

	// Return a pointer to LinkValue holding all the messages.
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(MessagesOfLink)
#define createMessagesOfLink CREATE_DECL(MessagesOfLink)

/** @}*/
}

#endif // _OPENCOG_MESSAGES_OF_LINK_H
