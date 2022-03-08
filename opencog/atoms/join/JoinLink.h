/*
 * opencog/atoms/join/JoinLink.h
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

/// Allow users to overload the getIncomingSet method.
/// Currently required by the file-storage backend.
class JoinCallback
{
public:
	virtual ~JoinCallback() {}

	/// Callback to get the IncomgingSet of the given Handle.
	virtual IncomingSet get_incoming_set(const Handle&) = 0;
};

class JoinLink : public PrenexLink
{
protected:
	void init(void);

	void validate(void);

	// Terms without variable declarations in them.
	HandleSet _const_terms;

	// The base from which we will work upwards from.
	Handle _meet;
	void setup_meet(void);

	// Total number of terms to join together.
	size_t _jsize;
	size_t _vsize;

	// A named top-variable, if it exists
	Handle _top_var;
	HandleSeq _top_clauses;
	bool _need_top_map;
	void setup_top_clauses(void);

	// A list of type-checks to be applied to the top.
	HandleSeq _top_types;
	void setup_top_types(void);

	// Traversal context
	struct Traverse
	{
		JoinCallback *jcb;
		HandleSet containers;
		HandleMap replace_map;
		HandleSetSeq join_map;
		HandleSeqMap top_map;
	};

	HandleSet principals(AtomSpace*, Traverse&) const;
	void principal_filter(Traverse&, HandleSet&, const Handle&) const;
	void principal_filter_map(Traverse&, const HandleSeq&,
	                          HandleSet&, const Handle&) const;

	HandleSet upper_set(AtomSpace*, bool, Traverse&) const;
	HandleSet supremum(AtomSpace*, bool, Traverse&) const;

	HandleSet constrain(AtomSpace*, bool, Traverse&) const;

	void fixup_replacements(Traverse&) const;
	HandleSet replace(const Traverse&) const;

	void find_top(Traverse&, const Handle&) const;
	HandleSet container(AtomSpace*, JoinCallback*, bool) const;

	virtual QueueValuePtr do_execute(AtomSpace*,
	                                 JoinCallback*,  bool silent);

public:
	JoinLink(const HandleSeq&&, Type=JOIN_LINK);

	JoinLink(const JoinLink&) = delete;
	JoinLink operator=(const JoinLink&) = delete;

	virtual bool is_executable() const { return true; }
	virtual ValuePtr execute(AtomSpace*, bool);

	ValuePtr execute_cb(AtomSpace*, JoinCallback*);

	static Handle factory(const Handle&);
};

LINK_PTR_DECL(JoinLink)
#define createJoinLink CREATE_DECL(JoinLink)

/** @}*/
}

#endif // _OPENCOG_JOIN_LINK_H
