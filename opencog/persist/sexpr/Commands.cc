/*
 * Commands.cc
 * Fast command interpreter for basic AtomSpace commands.
 *
 * Copyright (C) 2020 Linas Vepstas
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

#include <functional>
#include <string>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Commands.h"
#include "Sexpr.h"

using namespace opencog;

std::string Commands::interpret_command(AtomSpace* as,
                                        const std::string& cmd)
{
	// Fast dispatch. There should be zero hash collisions
	// here. If there are, we are in trouble. (Well, if there
	// are collisions, pre-pent the paren, post-pend the space.)
	static const size_t clear = std::hash<std::string>{}("cog-atomspace-clear)");
	static const size_t cache = std::hash<std::string>{}("cog-execute-cache!");
	static const size_t extra = std::hash<std::string>{}("cog-extract!");
	static const size_t recur = std::hash<std::string>{}("cog-extract-recursive!");
	static const size_t gtatm = std::hash<std::string>{}("cog-get-atoms");
	static const size_t incty = std::hash<std::string>{}("cog-incoming-by-type");
	static const size_t incom = std::hash<std::string>{}("cog-incoming-set");
	static const size_t keys = std::hash<std::string>{}("cog-keys->alist");
	static const size_t link = std::hash<std::string>{}("cog-link");
	static const size_t node = std::hash<std::string>{}("cog-node");
	static const size_t stval = std::hash<std::string>{}("cog-set-value!");
	static const size_t svals = std::hash<std::string>{}("cog-set-values!");
	static const size_t settv = std::hash<std::string>{}("cog-set-tv!");
	static const size_t value = std::hash<std::string>{}("cog-value");

	// Find the command and dispatch
	size_t pos = cmd.find_first_not_of(" \n\t");
	if (std::string::npos == pos) return "";

	// Ignore comments
	if (';' == cmd[pos]) return "";

	if ('(' != cmd[pos])
		throw SyntaxException(TRACE_INFO, "Badly formed command: %s",
			cmd.c_str());

	pos ++; // Skip over the open-paren

	size_t epos = cmd.find_first_of(" \n\t", pos);
	if (std::string::npos == epos)
		throw SyntaxException(TRACE_INFO, "Not a command: %s",
			cmd.c_str());

	size_t act = std::hash<std::string>{}(cmd.substr(pos, epos-pos));

	// (cog-atomspace-clear)
	if (clear == act)
	{
		as->clear();
		return "#t\n";
	}

	//    cog-execute-cache!
	if (cache == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-extract!
	if (extra == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-extract-recursive!
	if (recur == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	// (cog-get-atoms 'Node #t)
	if (gtatm == act)
	{
		pos = epos + 1;
		size_t nos = cmd.find_first_of(") \n\t", pos);
		size_t sos = nos;
		if ('\'' == cmd[pos]) pos++;
		if ('"' == cmd[pos]) { pos++; sos--; }

		Type t = nameserver().getType(cmd.substr(pos, sos-pos));
		if (NOTYPE == t)
			throw SyntaxException(TRACE_INFO, "Unknown Type >>%s<<",
				cmd.substr(pos, sos-pos).c_str());

		pos = cmd.find_first_not_of(") \n\t", nos);
		bool get_subtypes = false;
		if (std::string::npos != pos and cmd.compare(pos, 2, "#f"))
			get_subtypes = true;

		std::string rv = "(";
		HandleSet hset;
		as->get_handleset_by_type(hset, t, get_subtypes);
		for (const Handle& h: hset)
			rv += Sexpr::encode_atom(h);
		rv += ")";
		return rv;
	}

	//    cog-incoming-by-type
	if (incty == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-incoming-set
	if (incom == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	// (cog-keys->alist (Concept "foo"))
	if (keys == act)
	{
		pos = epos + 1;
		Handle h = Sexpr::decode_atom(cmd, pos);
		h = as->add_atom(h);
		std::string alist = "(";
		for (const Handle& key : h->getKeys())
		{
			alist += "(" + Sexpr::encode_atom(key) + " . ";
			alist += Sexpr::encode_value(h->getValue(key)) + ")";
		}
		alist += ")\n";
		return alist;
	}

	// (cog-node 'Concept "foobar")
	// (cog-link 'ListLink (Atom) (Atom) (Atom))
	if (node == act or link == act)
	{
		pos = epos + 1;
		size_t nos = cmd.find_first_of(" \n\t", pos);
		size_t sos = nos;
		if ('\'' == cmd[pos]) pos++;
		if ('"' == cmd[pos]) { pos++; sos--; }

		Type t = nameserver().getType(cmd.substr(pos, sos-pos));
		if (NOTYPE == t)
			throw SyntaxException(TRACE_INFO, "Unknown Type >>%s<<",
				cmd.substr(pos, sos-pos).c_str());

		Handle h;
		if (node == act)
		{
			pos = cmd.find('"', nos+1) + 1;
			nos = cmd.find('"', pos);
			h = as->get_node(t, cmd.substr(pos, nos-pos));
		}
		else
		if (link == act)
		{
			HandleSeq outgoing;
			size_t l = nos+1;
			size_t r = cmd.size();
			while (l < r and ')' != cmd[l])
			{
				size_t l1 = l;
				size_t r1 = r;
				Sexpr::get_next_expr(cmd, l1, r1, 0);
				if (l1 == r1) break;
				outgoing.push_back(Sexpr::decode_atom(cmd, l1, r1, 0));
				l = r1 + 1;
			}
			h = as->get_link(t, std::move(outgoing));
		}
		if (nullptr == h) return "()\n";
		return Sexpr::encode_atom(h);
	}

	// (cog-set-value! (Concept "foo") (Predicate "key") (FloatValue 1 2 3))
	if (stval == act)
	{
		pos = epos + 1;
		Handle atom = Sexpr::decode_atom(cmd, pos);
		atom = as->add_atom(atom);
		Handle key = Sexpr::decode_atom(cmd, ++pos);
		key = as->add_atom(key);
		ValuePtr vp = Sexpr::decode_value(cmd, ++pos);
		if (vp)
			vp = Sexpr::add_atoms(as, vp);
		atom->setValue(key, vp);
		return "()\n";
	}

	// (cog-set-values! (Concpet "foo")
	//     (list (cons (Predicate "bar") (stv 0.9 0.8)) ...))
	if (svals == act)
	{
		pos = epos + 1;
		Handle h = as->add_atom(Sexpr::decode_atom(cmd, pos));
		pos++; // skip past close-paren
		Sexpr::decode_slist(h, cmd, pos);
		return "()\n";
	}

	// (cog-set-tv! (Concept "foo") (stv 1 0))
	if (settv == act)
	{
		pos = epos + 1;
		Handle h = Sexpr::decode_atom(cmd, pos);
		as->add_atom(h);
		return "()\n";
	}

	//    cog-value
	if (value == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	throw SyntaxException(TRACE_INFO, "Command not supported: >>%s<<",
		cmd.substr(pos, epos-pos).c_str());
}
