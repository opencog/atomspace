/*
 * opencog/persist/api/BackingQuery.cc
 *
 * Copyright (c) 2020 Linas Vepstas <linas@linas.org>
 * All Rights Reserved
 *
 * LICENSE:
 * SPDX-License-Identifier: AGPL-3.0-or-later
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

#include <time.h>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspace/BackingStore.h>
#include <opencog/atomspace/Transient.h>

#include <opencog/atoms/container/JoinLink.h>
#include <opencog/atoms/pattern/QueryLink.h>
#include <opencog/query/Implicator.h>
#include <opencog/query/Satisfier.h>

// This is in a C file, not a header file,
// because no else one should touch this.
namespace opencog
{

// Callback for QueryLinks
class BackingImplicator : public Implicator
{
		BackingStore* _store;
		AtomSpace* _ras;
	public:
		BackingImplicator(BackingStore* sto, AtomSpace* as) :
			Implicator(as), _store(sto), _ras(as) {}
		virtual ~BackingImplicator() {}
		virtual IncomingSet get_incoming_set(const Handle&, Type);
};

// Callback for MeetLinks
class BackingSatisfyingSet : public SatisfyingSet
{
		BackingStore* _store;
	public:
		BackingSatisfyingSet(BackingStore* sto, AtomSpace* as) :
			SatisfyingSet(as), _store(sto) {}
		virtual ~BackingSatisfyingSet() {}
		virtual IncomingSet get_incoming_set(const Handle&, Type);
};

// Callback for JoinLinks
class BackingJoinCallback : public JoinCallback
{
		BackingStore* _store;
		AtomSpace* _as;
	public:
		BackingJoinCallback(BackingStore* sto, AtomSpace* as)
			: _store(sto), _as(as) {}
		virtual ~BackingJoinCallback() {}
		virtual IncomingSet get_incoming_set(const Handle&);
};

} // namespace opencog

using namespace opencog;

// ==========================================================

void BackingStore::getIncomingSet(AtomSpace* as, const Handle& h)
{
	getIncomingSet(as->get_atomtable(), h);
}

void BackingStore::getIncomingByType(AtomSpace* as, const Handle& h, Type t)
{
	getIncomingByType(as->get_atomtable(), h, t);
}

// ==========================================================

IncomingSet BackingImplicator::get_incoming_set(const Handle& h, Type t)
{
	_store->getIncomingByType(_ras, h, t);
	return h->getIncomingSetByType(t, _ras);
}

IncomingSet BackingSatisfyingSet::get_incoming_set(const Handle& h, Type t)
{
	_store->getIncomingByType(_as, h, t);
	return h->getIncomingSetByType(t, _as);
}

IncomingSet BackingJoinCallback::get_incoming_set(const Handle& h)
{
	_store->getIncomingSet(_as, h);
	return h->getIncomingSet(_as);
}

// ==========================================================

/// Attention: The design of this thing is subject to change.
/// This is the current experimental API.
///
/// The thing I don't like about this is the caching... so, we
/// performed the query straight out of disk starage (with the
/// incoming-set trick above), but then we are wasting CPU cycles
/// writing results back to disk, and maybe the user didn't need
/// that.
///
/// This should be compared to the client-server variant of this,
/// where the caching comes "for free" (because the search result is
/// already on the srever; it would take more effort to send it to the
/// client, delete it from the server, only to have the client turn
/// around and send it back to the server for caching.
///
/// Maybe we need two versions of this: a cached and a non-cached API...
void BackingStore::runQuery(const Handle& query, const Handle& key,
                            const Handle& meta, bool fresh)
{
	Type qt = query->get_type();
	if (not nameserver().isA(qt, MEET_LINK) and
	    not nameserver().isA(qt, QUERY_LINK) and
	    not nameserver().isA(qt, JOIN_LINK))
	{
		throw IOException(TRACE_INFO,
			"For now, only Meet, Join and Query are supported!");
	}

	if (not fresh)
	{
		// Return cached value, by default.
		ValuePtr vp = query->getValue(key);
		if (vp != nullptr) return;

		// Oh no! Go fetch it!
		loadValue(query, key);
		if (meta) loadValue(query, meta);
		barrier();
		ValuePtr lvp = query->getValue(key);
		if (lvp != nullptr) return;
	}

	// Still no luck. Bummer. Perform the query.
	AtomSpace* as = query->getAtomSpace();

	ValuePtr qv;
	if (nameserver().isA(qt, QUERY_LINK))
	{
		QueryLinkPtr qlp(QueryLinkCast(query));

		AtomSpace* tas = grab_transient_atomspace(as);
		BackingImplicator impl(this, tas);
		impl.implicand = qlp->get_implicand();
		impl.satisfy(qlp);

		qv = impl.get_result_queue();
		release_transient_atomspace(tas);
	}
	else if (nameserver().isA(qt, MEET_LINK))
	{
		AtomSpace* tas = grab_transient_atomspace(as);
		BackingSatisfyingSet sater(this, tas);
		sater.satisfy(PatternLinkCast(query));

		qv = sater.get_result_queue();
		release_transient_atomspace(tas);
	}
	else if (nameserver().isA(qt, JOIN_LINK))
	{
		AtomSpace* tas = grab_transient_atomspace(as);
		BackingJoinCallback rjcb(this, tas);

		qv = JoinLinkCast(query)->execute_cb(tas, &rjcb);
		release_transient_atomspace(tas);
	}
	else
	{
		throw IOException(TRACE_INFO, "Unsupported query type %s",
			nameserver().getTypeName(qt).c_str());
	}

	// Copy Atoms out of the transient AtomSpace.
	if (qv) qv = as->add_atoms(qv);
	query->setValue(key, qv);

	// And cache it in the file, as well! This caching is compatible
	// with what `cog-execute-cache!` does. It allows the cached
	// value to be retreived later, without re-performing the search.
	storeValue(query, key);

	// If there's a meta-info key, then attach a timestamp. For now,
	// that's the only meta info we attach, and we try to be compatible
	// with what the code in `cog-execute-cache!` does. See
	// https://github.com/opencog/atomspace/tree/master/opencog/scm/opencog/exec.scm
   // somewhere around lines 16-50.

	if (nullptr == meta) return;

	time_t now = time(0);
	double dnow = now;
	query->setValue(meta, createFloatValue(dnow));
	storeValue(query, meta);
}

// ====================== END OF FILE =======================
