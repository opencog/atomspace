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
/// `Commands::interpret_command()` below.  The goal is to avoid the
/// overhead of entry/exit into guile. This works because the cogserver
/// is guaranteed to send only these commands, and no others.
//

Dispatcher::Dispatcher(void)
{
	// Fast dispatch. There should be zero hash collisions
	// here. If there are, we are in trouble. (Well, if there
	// are collisions, pre-pend the paren, post-pend the space.)
	static const size_t space = std::hash<std::string>{}("cog-atomspace)");
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
	static const size_t dfine = std::hash<std::string>{}("define");
	static const size_t ping = std::hash<std::string>{}("ping)");
	static const size_t versn = std::hash<std::string>{}("cog-version)");

	using namespace std::placeholders;  // for _1, _2, _3...

	// Hash map to look up method to call
	_dispatch_map.insert({space, std::bind(&Dispatcher::cog_atomspace, this, _1)});
	_dispatch_map.insert({clear, std::bind(&Dispatcher::cog_atomspace_clear, this, _1)});
	_dispatch_map.insert({cache, std::bind(&Dispatcher::cog_execute_cache, this, _1)});
	_dispatch_map.insert({extra, std::bind(&Dispatcher::cog_extract, this, _1)});
	_dispatch_map.insert({recur, std::bind(&Dispatcher::cog_extract_recursive, this, _1)});

	_dispatch_map.insert({gtatm, std::bind(&Dispatcher::cog_get_atoms, this, _1)});
	_dispatch_map.insert({incty, std::bind(&Dispatcher::cog_incoming_by_type, this, _1)});
	_dispatch_map.insert({incom, std::bind(&Dispatcher::cog_incoming_set, this, _1)});
	_dispatch_map.insert({keys, std::bind(&Dispatcher::cog_keys_alist, this, _1)});
	_dispatch_map.insert({link, std::bind(&Dispatcher::cog_link, this, _1)});
	_dispatch_map.insert({node, std::bind(&Dispatcher::cog_node, this, _1)});

	_dispatch_map.insert({stval, std::bind(&Dispatcher::cog_set_value, this, _1)});
	_dispatch_map.insert({svals, std::bind(&Dispatcher::cog_set_values, this, _1)});
	_dispatch_map.insert({settv, std::bind(&Dispatcher::cog_set_tv, this, _1)});
	_dispatch_map.insert({value, std::bind(&Dispatcher::cog_value, this, _1)});
	_dispatch_map.insert({dfine, std::bind(&Dispatcher::cog_define, this, _1)});
	_dispatch_map.insert({ping, std::bind(&Dispatcher::cog_ping, this, _1)});
	_dispatch_map.insert({versn, std::bind(&Dispatcher::cog_version, this, _1)});
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

// -----------------------------------------------
// Frame support
void Dispatcher::set_base_space(const AtomSpacePtr& asp)
{
	_default.set_base_space(asp);
}

// -----------------------------------------------
// (cog-atomspace)
std::string Dispatcher::cog_atomspace(const std::string& arg)
{
	return _default.cog_atomspace(arg);
}

// -----------------------------------------------
// (cog-atomspace-clear)
std::string Dispatcher::cog_atomspace_clear(const std::string& arg)
{
	return _default.cog_atomspace_clear(arg);
}

// -----------------------------------------------
// (cog-execute-cache! (GetLink ...) (Predicate "key") ...)
// This is complicated, and subject to change...
std::string Dispatcher::cog_execute_cache(const std::string& cmd)
{
	return _default.cog_execute_cache(cmd);
}

// -----------------------------------------------
// (cog-extract! (Concept "foo"))
std::string Dispatcher::cog_extract(const std::string& cmd)
{
	return _default.cog_extract(cmd);
}

// -----------------------------------------------
// (cog-extract-recursive! (Concept "foo"))
std::string Dispatcher::cog_extract_recursive(const std::string& cmd)
{
	return _default.cog_extract_recursive(cmd);
}

// -----------------------------------------------
// (cog-get-atoms 'Node #t)
std::string Dispatcher::cog_get_atoms(const std::string& cmd)
{
	return _default.cog_get_atoms(cmd);
}

// -----------------------------------------------
// (cog-incoming-by-type (Concept "foo") 'ListLink)
std::string Dispatcher::cog_incoming_by_type(const std::string& cmd)
{
	return _default.cog_incoming_by_type(cmd);
}

// -----------------------------------------------
// (cog-incoming-set (Concept "foo"))
std::string Dispatcher::cog_incoming_set(const std::string& cmd)
{
	return _default.cog_incoming_set(cmd);
}

// -----------------------------------------------
// (cog-keys->alist (Concept "foo"))
std::string Dispatcher::cog_keys_alist(const std::string& cmd)
{
	return _default.cog_keys_alist(cmd);
}

// -----------------------------------------------
// (cog-node 'Concept "foobar")
std::string Dispatcher::cog_node(const std::string& cmd)
{
	return _default.cog_node(cmd);
}

// -----------------------------------------------
// (cog-link 'ListLink (Atom) (Atom) (Atom))
std::string Dispatcher::cog_link(const std::string& cmd)
{
	return _default.cog_link(cmd);
}

// -----------------------------------------------
// (cog-set-value! (Concept "foo") (Predicate "key") (FloatValue 1 2 3))
std::string Dispatcher::cog_set_value(const std::string& cmd)
{
	return _default.cog_set_value(cmd);
}

// -----------------------------------------------
// (cog-set-values! (Concept "foo") (AtomSpace "foo")
//     (alist (cons (Predicate "bar") (stv 0.9 0.8)) ...))
std::string Dispatcher::cog_set_values(const std::string& cmd)
{
	return _default.cog_set_values(cmd);
}

// -----------------------------------------------
// (cog-set-tv! (Concept "foo") (stv 1 0))
// (cog-set-tv! (Concept "foo") (stv 1 0) (AtomSpace "foo"))
std::string Dispatcher::cog_set_tv(const std::string& cmd)
{
	return _default.cog_set_tv(cmd);
}

// -----------------------------------------------
// (cog-update-value! (Concept "foo") (Predicate "key") (FloatValue 1 2 3))
std::string Dispatcher::cog_update_value(const std::string& cmd)
{
	return _default.cog_update_value(cmd);
}

// -----------------------------------------------
// (cog-value (Concept "foo") (Predicate "key"))
std::string Dispatcher::cog_value(const std::string& cmd)
{
	return _default.cog_value(cmd);
}

// -----------------------------------------------
// (define sym (AtomSpace "foo" (AtomSpace "bar") (AtomSpace "baz")))
// Place the current atomspace at the bottom of the hierarchy.
std::string Dispatcher::cog_define(const std::string& cmd)
{
	return _default.cog_define(cmd);
}

// -----------------------------------------------
// (ping) -- network ping
std::string Dispatcher::cog_ping(const std::string& cmd)
{
	return _default.cog_ping(cmd);
}

// -----------------------------------------------
// (cog-version) -- AtomSpace version
std::string Dispatcher::cog_version(const std::string& cmd)
{
	return _default.cog_version(cmd);
}

// ===================================================================
