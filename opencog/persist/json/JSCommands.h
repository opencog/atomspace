/*
 * JSCommands.h
 * Minimalist JSON command interpreter.
 *
 * Copyright (C) 2020, 2021 Linas Vepstas
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

#ifndef _JS_COMMANDS_H
#define _JS_COMMANDS_H

#include <string>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class AtomSpace;

class JSCommands
{
public:
	/// Interpret a handful of Javascript function calls.
	/// This is an ultra-minimalistic command interpreter. It only
	/// supports those commands needed for network I/O of AtomSpace
	/// contents (The cogserver uses this to provide peer AtomSpace
	/// network services). The goal is to provide just enough stuff
	/// to allow WebApps to be developed. 
	///
	/// Supported function calls:
	///    AtomSpace.getAtoms(type, recursive)
	///    AtomSpace.haveNode(type, name)
	///    AtomSpace.haveLink(type, outgoing)
	///    AtomSpace.haveAtom(atom)
	///    AtomSpace.getIncoming(atom)
	///    AtomSpace.getIncoming(atom, type)
	///    AtomSpace.getValues(atom)
	///
	/// So far, there aren't any commands to change the contents of
	/// the atomspace, but there could be ... these aren't hard.
	/// Sp far, the query command is not supported. It could be,
	/// its really easy. See `../sexpr/Commands.cc` for examples.
	///
	static std::string interpret_command(AtomSpace*, const std::string&);
};

/** @}*/
} // namespace opencog

#endif // _JS_COMMANDS_H
