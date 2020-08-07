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
		virtual Handle get_link(const Handle&, Type, HandleSeq&&);
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
	_store->barrier();
	return h->getIncomingSetByType(t, _ras);
}

IncomingSet BackingSatisfyingSet::get_incoming_set(const Handle& h, Type t)
{
	_store->getIncomingByType(_as, h, t);
	_store->barrier();
	return h->getIncomingSetByType(t, _as);
}

Handle BackingSatisfyingSet::get_link(const Handle& hg,
                                      Type t, HandleSeq&& oset)
{
	Handle h = _store->getLink(t, oset);
	if (nullptr == h) return h;
	return _as->add_atom(h);
}

IncomingSet BackingJoinCallback::get_incoming_set(const Handle& h)
{
	_store->getIncomingSet(_as, h);
	_store->barrier();
	return h->getIncomingSet(_as);
}

// ==========================================================

/// runQuery -- run a specific query on the backend dataset and load
/// only those atoms that satisfy the query. (i.e. load them into the
/// AtomSpace.)
///
/// This is currently experimental, and subject to change.
///
/// The thing I don't like about this is the caching design...
/// So: first of all, there are *two* generic kinds of backends, call
/// them "local" and "remote". The "remote" backends can find Atoms
/// easily enough, but have a high cost of shipping them (i.e. over the
/// network). The "local" backends have a low or zero cost of shipping
/// Atoms; the main bottleneck is just obtaining the Atoms in  the first
/// place.  The caching needs of these two backends differ.
///
/// The caching currently works as follows:
/// On the first call, the query is performed, and the resulting Atoms
/// are loaded into the AtomSpace, and they are *also* attached to the
/// indicated key.  That's the cache. On second and subsequent calls,
/// the value on the key is returned (thus avoiding the over-head of
/// re-running the query.) The cache can be cleared by deleting the key.
///
/// For the "local storage" case, the caching "isn't really needed",
/// because the user can "do it themselves" -- its not hard -- run the
/// query, attach the results to the key. Done.  There's even a
/// downside: the code below writes that key back into storage,
/// potentially wasting both CPU time, and storage space.  This is
/// bothersome, if the user didn't need that, and was just wanted
/// throw-away results.
///
/// For the "remote storage" case, the below is much closer to ideal.
/// Asking users to "do it themselves" for the remote case is
/// inefficient. The query is computed on the remote server, and is
/// shipped to the local server. The "do-it-yourself" cases would then
/// ship the query results back to the server... which is a pointless
/// waste of network bandwidth and risks extra latency.
///
/// So, I'm thinking, ... Maybe we need two versions of this: a cached
/// and a non-cached API... or maybe one more flag-- an "always cache
/// remotely" flag...
///
/// See also the notes about meta-information. It's currently
/// implemented to be compatible with what `cog-execute-cache!` does.
/// (and that compatibility should be maintained). See
/// `opencog/scm/opencog/exec.scm` for that code.
///
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
