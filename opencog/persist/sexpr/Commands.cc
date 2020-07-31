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
	static const size_t clear = std::hash<std::string>{}("cog-atomspace-clear");
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
	
	size_t pos = cmd.find_first_of(" \n\t", 1);
	if (std::string::npos == pos)
		throw SyntaxException(TRACE_INFO, "Not a command %s",
			cmd.c_str());

	size_t act = std::hash<std::string>{}(cmd.substr(1, pos-1));
	pos++;

	//    cog-atomspace-clear
	if (clear == act)
	{
		as->clear();
		return "#t";
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

	//    cog-get-atoms
	if (gtatm == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-incoming-by-type
	if (incty == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-incoming-set
	if (incom == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-keys->alist
	if (keys == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	// (cog-node 'Concept "foobar")
	// (cog-link 'ListLink (Atom) (Atom) (Atom))
	if (node == act or link == act)
	{
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
		if (nullptr == h) return "()";
		return Sexpr::encode_atom(h);
	}

	//    cog-set-value!
	if (stval == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-set-values!
	if (svals == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-set-tv!
	if (settv == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	//    cog-value
	if (value == act)
		throw SyntaxException(TRACE_INFO, "Not implemented");

	throw SyntaxException(TRACE_INFO, "Command not supported: >>%s<<",
		cmd.substr(1, pos-1).c_str());
}
