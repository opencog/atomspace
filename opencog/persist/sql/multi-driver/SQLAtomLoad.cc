/*
 * SQLAtomLoad.cc
 * Restore of individual atoms.
 *
 * Copyright (c) 2008,2009,2013,2017 Linas Vepstas <linas@linas.org>
 *
 * LICENSE:
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
#include <unistd.h>

#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atomspaceutils/TLB.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

using namespace opencog;

#define BUFSZ 250

/* ================================================================ */
/**
 * One-size-fits-all atom fetcher.
 * Given an SQL query string, this will return a single atom.
 * It does NOT fetch values.
 */
SQLAtomStorage::PseudoPtr SQLAtomStorage::getAtom(const char * query, int height)
{
	Response rp(conn_pool);
	rp.uuid = TLB::INVALID_UUID;
	rp.exec(query);
	rp.rs->foreach_row(&Response::create_atom_cb, &rp);

	// Did we actually find anything?
	// DO NOT USE IsInvalidHandle() HERE! It won't work, duhh!
	if (rp.uuid == TLB::INVALID_UUID) return nullptr;

	rp.height = height;
	PseudoPtr atom(makeAtom(rp, rp.uuid));
	return atom;
}

SQLAtomStorage::PseudoPtr SQLAtomStorage::petAtom(UUID uuid)
{
	setup_typemap();
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Atoms WHERE uuid = %lu;", uuid);

	return getAtom(buff, -1);
}

/// Get the full outgoing set, recursively.
/// When adding links of unknown provenance, it could happen that
/// the outgoing set of the link has not yet been loaded.  In
/// that case, we have to load the outgoing set first.
///
/// Note that this does NOT fetch any values!
Handle SQLAtomStorage::get_recursive_if_not_exists(PseudoPtr p)
{
	if (nameserver().isA(p->type, NODE))
	{
		Handle h(_tlbuf.getAtom(p->uuid));
		if (h) return h;

		Handle node(createNode(p->type, std::move(p->name)));
		_tlbuf.addAtom(node, p->uuid);
		_num_rec_nodes ++;
		return node;
	}
	HandleSeq resolved_oset;
	for (UUID idu : p->oset)
	{
		Handle h(_tlbuf.getAtom(idu));
		if (h)
		{
			resolved_oset.emplace_back(h);
			continue;
		}
		PseudoPtr po(petAtom(idu));

		// Corrupted databases can have outoging sets that refer
		// to non-existent atoms. This is rare, but has happened.
		// Without this check, the null-pointer deref will crash.
		if (nullptr == po)
			throw IOException(TRACE_INFO,
				"SQLAtomStorage::get_recursive_if_not_exists: "
				"Corrupt database; no atom for uuid=%lu", idu);

		Handle ha(get_recursive_if_not_exists(po));
		resolved_oset.emplace_back(ha);
	}
	Handle link(createLink(std::move(resolved_oset), p->type));
	_tlbuf.addAtom(link, p->uuid);
	_num_rec_links++;
	return link;
}

/**
 * Fetch the Node with the indicated type and name.
 * If there is no such node, NULL is returned.
 */
Handle SQLAtomStorage::doGetNode(Type t, const char * str)
{
	// First, check to see if we already know this Node.
	Handle node(createNode(t, str));
	UUID uuid = _tlbuf.getUUID(node);
	if (TLB::INVALID_UUID != uuid)
		return _tlbuf.getAtom(uuid);

	// If we don't know it, then go get it's UUID.
	setup_typemap();
	char buff[4*BUFSZ];

	// Use postgres $-quoting to make unicode strings easier to deal with.
	int nc = snprintf(buff, 4*BUFSZ, "SELECT * FROM Atoms WHERE "
		"type = %d AND name = $ocp$%s$ocp$ ;", storing_typemap[t], str);

	if (4*BUFSZ-1 <= nc)
	{
		buff[4*BUFSZ-1] = 0x0;
		throw IOException(TRACE_INFO,
			"SQLAtomStorage::getNode: buffer overflow!\n"
			"\tnc=%d buffer=>>%s<<\n", nc, buff);
		return Handle();
	}

	// Performance stats
	_num_get_nodes++;

	PseudoPtr p(getAtom(buff, 0));
	if (NULL == p) return Handle();

	_num_got_nodes++;
	_tlbuf.addAtom(node, p->uuid);
	return _tlbuf.getAtom(p->uuid);
}

Handle SQLAtomStorage::getNode(Type t, const char * str)
{
	rethrow();
	Handle h(doGetNode(t, str));
	if (h) get_atom_values(h);
	return h;
}

/**
 * Fetch the Link with given type and outgoing set.
 * If there is no such link, NULL is returned.
 */
Handle SQLAtomStorage::doGetLink(Type t, const HandleSeq& hseq)
{
	// First, check to see if we already know this Link.
	Handle link(createLink(std::move(HandleSeq(hseq)), t));
	UUID uuid = _tlbuf.getUUID(link);
	if (TLB::INVALID_UUID != uuid)
		return _tlbuf.getAtom(uuid);

	// If the outgoing set is not yet known, then the link
	// itself cannot possibly be known. The oset_to_string()
	// will throw; users must catch.
	std::string ostr(oset_to_string(hseq));

	// If we don't know it, then go get it's UUID.
	setup_typemap();

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Atoms WHERE type = %d AND outgoing = ",
		storing_typemap[t]);

	std::string qstr = buff;
	qstr += ostr;
	qstr += ";";

	// Performance stats
	_num_get_links++;
	PseudoPtr p = getAtom(qstr.c_str(), 1);
	if (nullptr == p) return Handle();

	_num_got_links++;
	_tlbuf.addAtom(link, p->uuid);
	return _tlbuf.getAtom(p->uuid);
}

Handle SQLAtomStorage::getLink(Type t, const HandleSeq& hs)
{
	rethrow();
	try
	{
		Handle hg(doGetLink(t, hs));
		get_atom_values(hg);
		return hg;
	}
	catch (const NotFoundException& ex)
	{
		return Handle::UNDEFINED;
	}
}

/**
 * Instantiate a new atom, from the response buffer contents
 */
SQLAtomStorage::PseudoPtr SQLAtomStorage::makeAtom(Response &rp, UUID uuid)
{
	// Now that we know everything about an atom, actually construct one.
	Type realtype = loading_typemap[rp.itype];

	if (NOTYPE == realtype)
	{
		throw IOException(TRACE_INFO,
			"Fatal Error: OpenCog does not have a type called %s\n",
			db_typename[rp.itype]);
		return NULL;
	}

	PseudoPtr atom(createPseudo());

	// All height zero atoms are nodes,
	// All positive height atoms are links.
	// A negative height is "unknown" and must be checked.
	if ((0 == rp.height) or
		((-1 == rp.height) and nameserver().isA(realtype, NODE)))
	{
		atom->name = rp.name;
	}
	else
	{
		char *p = (char *) rp.outlist;
		while (p)
		{
			// Break if there are no more atoms in the outgoing set
			// or if the outgoing set is empty in the first place.
			if (*p == '}' or *p == '\0') break;
			UUID out(strtoul(p+1, &p, 10));
			atom->oset.emplace_back(out);
		}
	}

	// Give the atom the correct UUID. The AtomTable will need this.
	atom->type = realtype;
	atom->uuid = uuid;

	_load_count ++;
	if (bulk_load and _load_count%100000 == 0)
	{
		time_t secs = time(0) - bulk_start;
		double rate = ((double) _load_count) / secs;
		unsigned long kays = ((unsigned long) _load_count) / 1000;
		printf("\tLoaded %luK atoms in %d seconds (%d per second).\n",
			kays, (int) secs, (int) rate);
	}

	return atom;
}

/* ============================= END OF FILE ================= */
