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

#include <map>
#include <string>

#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class Commands
{
public:
	typedef std::function<void (const Handle&)> CB_H;
	typedef std::function<void (const Handle&, const TruthValuePtr&)> CB_HT;
	typedef std::function<void (const Handle&, const Handle&, const ValuePtr&)> CB_HHV;

protected:
	/// True, if the _space_map below is being used, and AtomSpaces need
	/// to be sent and received.
	bool _multi_space;

	/// Map from string AtomSpace names to the matching AtomSpacePtr's
	std::unordered_map<std::string, Handle> _space_map;

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

	/// Methods that implement each of the interpreted commands.
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

	std::string cog_set_value(const std::string&, CB_HHV=nullptr);
	std::string cog_set_values(const std::string&, CB_H=nullptr);
	std::string cog_set_tv(const std::string&, CB_HT=nullptr);
	std::string cog_update_value(const std::string&, CB_HHV=nullptr);
	std::string cog_value(const std::string&);
	std::string cog_define(const std::string&);
	std::string cog_ping(const std::string&);
	std::string cog_version(const std::string&);
};

/** @}*/
} // namespace opencog

#endif // _COMMANDS_H
