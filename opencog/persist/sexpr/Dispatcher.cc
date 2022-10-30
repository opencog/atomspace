/*
 * Dispatcher.cc
 * Dispatcher for a command interpreter for basic AtomSpace commands.
 *
 * Copyright (C) 2020, 2022 Linas Vepstas
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

#include <functional>
#include <iomanip>
#include <string>

#include "Dispatcher.h"
#include "Commands.h"

using namespace opencog;

/// The cogserver provides a network API to send/receive Atoms over
/// the internet. The actual API is that of the StorageNode (see the
/// wiki page https://wiki.opencog.org/w/StorageNode for details.)
/// The cogserver supports the full `StorageNode` API, and it uses
/// the code in this directory in order to make it fast.
///
/// To aid in performance, a very special set of about 15 scheme
/// functions have been hard-coded in C++, in the static function
/// `Dispatcher::interpret_command()` below.  The goal is to avoid the
/// overhead of entry/exit into guile. This works because the cogserver
/// is guaranteed to send only these commands, and no others.
//

Dispatcher::Dispatcher(void)
{
	// Fast dispatch. There should be zero hash collisions
	// here. If there are, we are in trouble. (Well, if there
	// are collisions, pre-pend the paren, post-pend the space.)
#define MASH(HSH,STR) \
   static const size_t HSH = std::hash<std::string>{}(STR);

	MASH(space, "cog-atomspace)");
	MASH(clear, "cog-atomspace-clear)");
	MASH(cache, "cog-execute-cache!");
	MASH(extra, "cog-extract!");
	MASH(recur, "cog-extract-recursive!");

	MASH(gtatm, "cog-get-atoms");
	MASH(incty, "cog-incoming-by-type");
	MASH(incom, "cog-incoming-set");
	MASH(keys,  "cog-keys->alist");
	MASH(link,  "cog-link");
	MASH(node,  "cog-node");

	MASH(stval, "cog-set-value!");
	MASH(svals, "cog-set-values!");
	MASH(settv, "cog-set-tv!");
	MASH(value, "cog-value");
	MASH(dfine, "define");
	MASH(ping,  "ping)");
	MASH(versn, "cog-version)");

	using namespace std::placeholders;  // for _1, _2, _3...

	// Hash map to look up method to call
#define DIS(HSH,CB) \
   _dispatch_map.insert({HSH, std::bind(&Commands::##CB, _default, _1)});

	DIS(space, cog_atomspace);
	DIS(clear, cog_atomspace_clear);
	DIS(cache, cog_execute_cache);
	DIS(extra, cog_extract);
	DIS(recur, cog_extract_recursive);

	DIS(gtatm, cog_get_atoms);
	DIS(incty, cog_incoming_by_type);
	DIS(incom, cog_incoming_set);
	DIS(keys,  cog_keys_alist);
	DIS(link,  cog_link);
	DIS(node,  cog_node);

	DIS(stval, cog_set_value);
	DIS(svals, cog_set_values);
	DIS(settv, cog_set_tv);
	DIS(value, cog_value);
	DIS(dfine, cog_define);
	DIS(ping,  cog_ping);
	DIS(versn, cog_version);
}

Dispatcher::~Dispatcher()
{
}

void Dispatcher::install_handler(const std::string& idstr, Meth handler)
{
	size_t idhash = std::hash<std::string>{}(idstr);
	_dispatch_map.insert_or_assign(idhash, handler);
}

// -----------------------------------------------

std::string Dispatcher::interpret_command(const std::string& cmd)
{
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

	// Look up the method to call, based on the hash of the command string.
	size_t action = std::hash<std::string>{}(cmd.substr(pos, epos-pos));
	const auto& disp = _dispatch_map.find(action);

	if (_dispatch_map.end() != disp)
	{
		Meth f = disp->second;
		pos = cmd.find_first_not_of(" \n\t", epos);
		if (cmd.npos != pos)
			return f(cmd.substr(pos));
		return f(""); // no arguments available.
	}

	throw SyntaxException(TRACE_INFO, "Command not supported: >>%s<<",
		cmd.substr(pos, epos-pos).c_str());
}

// ===================================================================
