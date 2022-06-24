/*
 * Commands.h
 * Minimalist command interpreter.
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

#ifndef _COMMANDS_H
#define _COMMANDS_H

#include <string>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class AtomSpace;

class Commands
{
protected:
	/// True, if the _space_map below is being used, and AtomSpaces need
	/// to be sent and received.
	bool _multi_space;

	/// Map from string AtomSpace names to the matching AtomSpacePtr's
	std::unordered_map<std::string, Handle> _space_map;

	/// Map to dispatch table
	typedef std::string (Commands::*Meth)(const std::string&);
	std::unordered_map<size_t, Meth> _dispatch_map;

	AtomSpace* get_opt_as(const std::string&, size_t&);

	/// AtomSpace to which all commands apply.
	AtomSpacePtr _base_space;

	/// If AtomSpace frames are in use, this points at the top-most
	/// frame. It is needed so that the automatic use-counting does
	/// not free the frame immediattely after it is created.
	AtomSpacePtr top_space;

public:
	Commands(void);
	~Commands();

	// Indicate which AtomSpace to use
	void set_base_space(const AtomSpacePtr&);

	/// Interpret a very small subset of singular scheme commands.
	/// This is an ultra-minimalistic command interpreter. It only
	/// supports those commands needed for network I/O of AtomSpace
	/// contents (The cogserver uses this to provide peer AtomSpace
	/// network services). The goal is to provide much higher
	/// performance than what is possible through the guile interfaces.
	///
	/// The supported commands are:
	///    cog-atomspace-clear
	///    cog-execute-cache!
	///    cog-extract!
	///    cog-extract-recursive!
	///    cog-get-atoms
	///    cog-incoming-by-type
	///    cog-incoming-set
	///    cog-keys->alist
	///    cog-link
	///    cog-node
	///    cog-set-value!
	///    cog-set-values!
	///    cog-set-tv!
	///    cog-value
	///    ping
	///
	/// They MUST appear only once in the string, at the very beginning,
	/// and they MUST be followed by valid Atomese s-expressions, and
	/// nothing else.
	///
	std::string interpret_command(const std::string&);

	/// Methods that implement each of the interpreted commands, above.
	std::string cog_atomspace(const std::string&);
	std::string cog_atomspace_clear(const std::string&);
	std::string cog_execute_cache(const std::string&);
	std::string cog_extract(const std::string&);
	std::string cog_extract_recursive(const std::string&);

	std::string cog_get_atoms(const std::string&);
	std::string cog_incoming_by_type(const std::string&);
	std::string cog_incoming_set(const std::string&);
	std::string cog_keys_alist(const std::string&);
	std::string cog_link(const std::string&);
	std::string cog_node(const std::string&);

	std::string cog_set_value(const std::string&);
	std::string cog_set_values(const std::string&);
	std::string cog_set_tv(const std::string&);
	std::string cog_value(const std::string&);
	std::string cog_define(const std::string&);
	std::string cog_ping(const std::string&);

	void install_handler(const std::string&, Meth);
};

/** @}*/
} // namespace opencog

#endif // _COMMANDS_H
