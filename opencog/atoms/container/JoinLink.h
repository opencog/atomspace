/*
 * opencog/atoms/pattern/JoinLink.h
 *
 * Copyright (C) 2019 Linas Vepstas
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
#ifndef _OPENCOG_JOIN_LINK_H
#define _OPENCOG_JOIN_LINK_H

#include <opencog/atoms/core/PrenexLink.h>
#include <opencog/atoms/value/QueueValue.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class JoinLink : public PrenexLink
{
protected:
	void init(void);

	void validate(void);

	// Terms without variable declarations in them.
	HandleSet _const_terms;

	// The base from which we will work upwards from.
	size_t _vsize;
	Handle _meet;
	void setup_meet(void);

	// A list of type-checks to be applied to the top.
	HandleSeq _top_types;
	void setup_top_types(void);

	// Traversal context
	struct Traverse
	{
		HandleMap replace_map;
		HandleSetSeq join_map;
	};

	HandleSet principals(AtomSpace*, Traverse&) const;
	void principal_filter(HandleSet&, const Handle&) const;

	HandleSet upper_set(AtomSpace*, bool, Traverse&) const;
	HandleSet supremum(AtomSpace*, bool, Traverse&) const;

	HandleSet constrain(AtomSpace*, bool, const HandleSet&) const;

	void fixup_replacements(Traverse&) const;
	HandleSet replace(const HandleSet&, const Traverse&) const;

	void find_top(HandleSet&, const Handle&) const;
	HandleSet container(AtomSpace*, bool) const;

	virtual QueueValuePtr do_execute(AtomSpace*, bool silent);

public:
	JoinLink(const HandleSeq&&, Type=JOIN_LINK);

	JoinLink(const JoinLink&) = delete;
	JoinLink operator=(const JoinLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<JoinLink> JoinLinkPtr;
static inline JoinLinkPtr JoinLinkCast(const Handle& h)
	{ AtomPtr a(h); return std::dynamic_pointer_cast<JoinLink>(a); }
static inline JoinLinkPtr JoinLinkCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<JoinLink>(a); }

#define createJoinLink std::make_shared<JoinLink>

/** @}*/
}

#endif // _OPENCOG_JOIN_LINK_H
