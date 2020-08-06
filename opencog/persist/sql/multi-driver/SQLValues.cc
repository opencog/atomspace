/*
 * SQLValues.cc
 * Save and restore of atom values.
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
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/base/Valuation.h>
#include <opencog/atoms/truthvalue/TruthValue.h>

#include "SQLAtomStorage.h"
#include "SQLResponse.h"

using namespace opencog;

/* ================================================================== */

std::string SQLAtomStorage::oset_to_string(const HandleSeq& out)
{
	bool not_first = false;
	std::string str = "\'{";
	for (const Handle& h : out)
	{
		if (not_first) str += ", ";
		not_first = true;
		str += std::to_string(get_uuid(h));
	}
	str += "}\'";
	return str;
}

std::string SQLAtomStorage::float_to_string(const FloatValuePtr& fvle)
{
	bool not_first = false;
	std::string str = "\'{";
	for (double v : fvle->value())
	{
		if (not_first) str += ", ";
		not_first = true;

		char buf[40];
		snprintf(buf, 40, "%20.17g", v);
		str += buf;
	}
	str += "}\'";
	return str;
}

std::string SQLAtomStorage::string_to_string(const StringValuePtr& svle)
{
	bool not_first = false;
	std::string str = "\'{";
	for (const std::string& v : svle->value())
	{
		if (not_first) str += ", ";
		not_first = true;
		str += v;
	}
	str += "}\'";
	return str;
}

std::string SQLAtomStorage::link_to_string(const LinkValuePtr& lvle)
{
	bool not_first = false;
	std::string str = "\'{";
	for (const ValuePtr& pap : lvle->value())
	{
		if (not_first) str += ", ";
		not_first = true;
		VUID vuid = storeValue(pap);
		str += std::to_string(vuid);
	}
	str += "}\'";
	return str;
}

/* ================================================================ */
#define BUFSZ 250

#define STMT(colname,val) { \
	if (notfirst) { cols += ", "; vals += ", "; } else notfirst = true; \
	cols += colname; \
	vals += val; \
}

#define STMTI(colname,ival) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%d", ival); \
	STMT(colname, buff); \
}

#define STMTF(colname,fval) { \
	char buff[BUFSZ]; \
	snprintf(buff, BUFSZ, "%22.16g", fval); \
	STMT(colname, buff); \
}

/* ================================================================ */

/// Delete the valuation, if it exists. This is required, in order
/// to prevent garbage from accumulating in the Values table.
/// It also simplifies, ever-so-slightly, the update of valuations.
void SQLAtomStorage::deleteValuation(const Handle& key, const Handle& atom)
{
	Response rp(conn_pool);
	deleteValuation(rp, get_uuid(key), get_uuid(atom));
}

void SQLAtomStorage::deleteValuation(Response& rp, UUID key_uid, UUID atom_uid)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Valuations WHERE key = %lu AND atom = %lu;",
		key_uid, atom_uid);

	rp.vtype = 0;
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_value_cb, &rp);

	if (LINK_VALUE == rp.vtype)
	{
		const char *p = rp.lnkval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			VUID vu = atol(p);
			deleteValue(vu);
			p = strchr(p, ',');
			if (p) p++;
		}
	}

	if (0 != rp.vtype)
	{
		snprintf(buff, BUFSZ,
			"DELETE FROM Valuations WHERE key = %lu AND atom = %lu;",
			key_uid, atom_uid);

		rp.exec(buff);
	}
}

/**
 * Store a valuation. Return an integer ID for that valuation.
 * Thread-safe.
 */
void SQLAtomStorage::storeValuation(const ValuationPtr& valn)
{
	storeValuation(valn->key(), valn->atom(), valn->value());
}

void SQLAtomStorage::storeValuation(const Handle& key,
                                    const Handle& atom,
                                    const ValuePtr& pap)
{
	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	// Get UUID from the TLB.
	UUID kuid;
	{
		// We must make sure the key is in the database BEFORE it
		// is used in any valuation; else a 'foreign key constraint'
		// error will be thrown.  And to do that, we must make sure
		// the store completes, before some other thread gets its
		// fingers on the key.
		std::lock_guard<std::mutex> create_lock(_valuation_mutex);
		kuid = check_uuid(key);
		if (TLB::INVALID_UUID == kuid)
		{
			do_store_atom(key);
			kuid = get_uuid(key);
		}
	}

	char kidbuff[BUFSZ];
	snprintf(kidbuff, BUFSZ, "%lu", kuid);

	char aidbuff[BUFSZ];
	UUID auid = get_uuid(atom);
	snprintf(aidbuff, BUFSZ, "%lu", auid);

	// The prior valuation, if any, will be deleted first,
	// and so an INSERT is sufficient to cover everything.
	// During races, the second user looses.
	cols = "INSERT INTO Valuations (";
	vals = ") VALUES (";
	coda = ") ON CONFLICT DO NOTHING;";
	STMT("key", kidbuff);
	STMT("atom", aidbuff);

	Type vtype = pap->get_type();
	STMTI("type", storing_typemap[vtype]);

	if (nameserver().isA(vtype, FLOAT_VALUE))
	{
		FloatValuePtr fvp = FloatValueCast(pap);
		std::string fstr = float_to_string(fvp);
		STMT("floatvalue", fstr);
	}
	else
	if (nameserver().isA(vtype, STRING_VALUE))
	{
		StringValuePtr fvp = StringValueCast(pap);
		std::string sstr = string_to_string(fvp);
		STMT("stringvalue", sstr);
	}
	else
	if (nameserver().isA(vtype, LINK_VALUE))
	{
		LinkValuePtr fvp = LinkValueCast(pap);
		std::string lstr = link_to_string(fvp);
		STMT("linkvalue", lstr);
	}

	std::string insert = cols + vals + coda;

	std::lock_guard<std::mutex> lck(_value_mutex[auid%NUMVMUT]);
	// Use a transaction, so that other threads/users see the
	// valuation update atomically. That is, two sets of
	// users/threads can safely set the same valuation at the same
	// time. A third thread will always see an appropriate valuation,
	// either the earlier one, or the newer one.
	Response rp(conn_pool);
	rp.exec("BEGIN;");

	// If there's an existing valuation, delete it.
	deleteValuation(rp, kuid, auid);

	rp.exec(insert.c_str());
	rp.exec("COMMIT;");

	_valuation_stores++;
}

// Almost a cut-n-paste of the above, but different.
SQLAtomStorage::VUID SQLAtomStorage::storeValue(const ValuePtr& pap)
{
	VUID vuid = _vuid_manager.get_uuid();

	bool notfirst = false;
	std::string cols;
	std::string vals;
	std::string coda;

	cols = "INSERT INTO Values (";
	vals = ") VALUES (";
	coda = ") ON CONFLICT DO NOTHING;";
	STMT("vuid", std::to_string(vuid));

	Type vtype = pap->get_type();
	STMTI("type", storing_typemap[vtype]);

	if (nameserver().isA(vtype, FLOAT_VALUE))
	{
		FloatValuePtr fvp = FloatValueCast(pap);
		std::string fstr = float_to_string(fvp);
		STMT("floatvalue", fstr);
	}
	else
	if (nameserver().isA(vtype, STRING_VALUE))
	{
		StringValuePtr fvp = StringValueCast(pap);
		std::string sstr = string_to_string(fvp);
		STMT("stringvalue", sstr);
	}
	else
	if (nameserver().isA(vtype, LINK_VALUE))
	{
		LinkValuePtr fvp = LinkValueCast(pap);
		std::string lstr = link_to_string(fvp);
		STMT("linkvalue", lstr);
	}

	std::string qry = cols + vals + coda;
	Response rp(conn_pool);
	rp.exec(qry.c_str());

	_value_stores++;
	return vuid;
}

/// Return a value, given by the VUID identifier, taken from the
/// Values table. If the value type is a link, then the full recursive
/// fetch is performed.
ValuePtr SQLAtomStorage::getValue(VUID vuid)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Values WHERE vuid = %lu;", vuid);
	return doGetValue(buff);
}

/// Return a value, given by indicated query buffer.
/// If the value type is a link, then the full recursive
/// fetch is performed.
ValuePtr SQLAtomStorage::doGetValue(const char * buff)
{
	Response rp(conn_pool);
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_value_cb, &rp);
   return doUnpackValue(rp);
}

/// Return a value, given by indicated query buffer.
/// If the value type is a link, then the full recursive
/// fetch is performed.
ValuePtr SQLAtomStorage::doUnpackValue(Response& rp)
{
	// Convert from databasse type to C++ runtime type
	Type vtype = loading_typemap[rp.vtype];

	// We expect rp.strval to be of the form
	// {aaa,"bb bb bb","ccc ccc ccc"}
	// Split it along the commas.
	if (vtype == STRING_VALUE)
	{
		std::vector<std::string> strarr;
		char *s = strdup(rp.strval);
		char *p = s;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			// String terminates at comma or close-brace.
			char * c = strchr(p, ',');
			if (c) *c = 0;
			else c = strchr(p, '}');
			if (c) *c = 0;

			// Wipe out quote marks
			if (*p == '"') p++;
			if (c and *(c-1) == '"') *(c-1) = 0;

			strarr.emplace_back(p);
			p = c;
			p++;
		}
		free(s);
		return createStringValue(strarr);
	}

	// We expect rp.fltval to be of the form
	// {1.1,2.2,3.3}
	if ((vtype == FLOAT_VALUE)
	    or nameserver().isA(vtype, TRUTH_VALUE))
	{
		std::vector<double> fltarr;
		char *p = (char *) rp.fltval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			double flt = strtod(p, &p);
			fltarr.emplace_back(flt);
			p++; // skip over  comma
		}
		if (vtype == FLOAT_VALUE)
			return createFloatValue(fltarr);
		else
			return ValueCast(TruthValue::factory(vtype, fltarr));
	}

	// We expect rp.lnkval to be a comma-separated list of
	// vuid's, which we then fetch recursively.
	if (vtype == LINK_VALUE)
	{
		std::vector<ValuePtr> lnkarr;
		const char *p = rp.lnkval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			VUID vu = atol(p);
			ValuePtr pap = getValue(vu);
			lnkarr.emplace_back(pap);
			p = strchr(p, ',');
			if (p) p++;
		}
		return createLinkValue(lnkarr);
	}

	throw IOException(TRACE_INFO, "Unexpected value type=%d", rp.vtype);
	return nullptr;
}

void SQLAtomStorage::deleteValue(VUID vuid)
{
	char buff[BUFSZ];
	snprintf(buff, BUFSZ, "SELECT * FROM Values WHERE vuid = %lu;", vuid);

	Response rp(conn_pool);
	rp.exec(buff);
	rp.rs->foreach_row(&Response::get_value_cb, &rp);

	// Perform a recursive delete, if necessary.
	// We expect rp.strval to be of the form
	// {81,82,83} -- Split it along the commas.
	if (rp.vtype == LINK_VALUE)
	{
		const char *p = rp.lnkval;
		if (p and *p == '{') p++;
		while (p)
		{
			if (*p == '}' or *p == '\0') break;
			VUID vu = atol(p);
			deleteValue(vu);
			p = strchr(p, ',');
			if (p) p++;
		}
	}

	snprintf(buff, BUFSZ, "DELETE FROM Values WHERE vuid = %lu;", vuid);
	rp.exec(buff);
}

/// Store ALL of the values associated with the atom.
void SQLAtomStorage::store_atom_values(const Handle& atom)
{
	HandleSet keys = atom->getKeys();
	for (const Handle& key: keys)
	{
		ValuePtr pap = atom->getValue(key);
		storeValuation(key, atom, pap);
	}

	// Special-case for TruthValues. Can we get rid of this someday?
	// Delete default TV's, else storage will get clogged with them.
	TruthValuePtr tv(atom->getTruthValue());
	if (tv->isDefaultTV()) deleteValuation(tvpred, atom);
}

/// Get ALL of the values associated with an atom.
void SQLAtomStorage::get_atom_values(Handle& atom)
{
	if (nullptr == atom) return;

	char buff[BUFSZ];
	snprintf(buff, BUFSZ,
		"SELECT * FROM Valuations WHERE atom = %lu;",
		get_uuid(atom));

	Response rp(conn_pool);
	rp.exec(buff);

	rp.store = this;
	rp.atom = atom;
	rp.table = nullptr;
	rp.rs->foreach_row(&Response::get_all_values_cb, &rp);
	rp.atom = nullptr;
}

void SQLAtomStorage::loadValue(const Handle& atom, const Handle& key)
{
	rethrow();
	if (nullptr == atom) return;
	try
	{
		char buff[BUFSZ];
		snprintf(buff, BUFSZ,
			"SELECT * FROM Valuations WHERE key = %lu AND atom = %lu;",
			get_uuid(key), get_uuid(atom));

		Response rp(conn_pool);
		rp.exec(buff);

		rp.store = this;
		rp.atom = atom;
		rp.table = nullptr;
		rp.rs->foreach_row(&Response::get_all_values_cb, &rp);
		rp.atom = nullptr;
	}
	catch (const NotFoundException& ex) {}
}

void SQLAtomStorage::storeValue(const Handle& atom, const Handle& key)
{
	rethrow();
	if (nullptr == atom) return;

	ValuePtr pap = atom->getValue(key);
	storeValuation(key, atom, pap);
}

/* ============================= END OF FILE ================= */
