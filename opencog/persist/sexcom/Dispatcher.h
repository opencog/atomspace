/*
 * Dispatcher.h
 * Dispatcher for command interpreter.
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

#ifndef _DISPATCHER_H
#define _DISPATCHER_H

#include <functional>
#include <string>

#include <opencog/persist/sexcom/Commands.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class AtomSpace;

class Dispatcher
{
public:
	// XXX FIXME: This is a terrible design for performance.
	// The std::bind call turns into seven!! stack frames of
	// unwraps befor the actual method is called. This is ...
	// horrific. We can replace with with a conventional
	// class of virtual methods.
	typedef std::function<std::string (const std::string&)> Meth;

protected:
	Commands _default;

	/// Map to dispatch table
	std::unordered_map<size_t, Meth> _dispatch_map;

public:
	Dispatcher(void);
	~Dispatcher();

	// Indicate which AtomSpace to use
	void set_base_space(const AtomSpacePtr& asp) {
		_default.set_base_space(asp); }

	/// Interpret a very small subset of singular scheme commands.
	/// This is an ultra-minimalistic command interpreter. It only
	/// supports those commands needed for network I/O of AtomSpace
	/// contents (The cogserver uses this to provide peer AtomSpace
	/// network services). The goal is to provide much higher
	/// performance than what is possible through the guile interfaces.
	///
	/// The supported commands are:
	///    cog-atomspace
	///    cog-atomspace-clear
	///    cog-set-proxy!
	///    cog-proxy-open
	///    cog-proxy-close
	///    cog-execute-cache!
	///
	///    cog-get-atoms
	///    cog-incoming-by-type
	///    cog-incoming-set
	///    cog-keys->alist
	///    cog-link
	///    cog-node
	///    cog-value
	///
	///    cog-extract!
	///    cog-extract-recursive!
	///    cog-set-value!
	///    cog-set-values!
	///    cog-set-tv!
	///    cog-update-value!
	///
	///    ping
	///    cog-version
	///
	/// They MUST appear only once in the string, at the very beginning,
	/// and they MUST be followed by valid Atomese s-expressions, and
	/// nothing else.
	///
	std::string interpret_command(const std::string&);

	/// Install a callback handler, over-riding the default behavior for
	/// the command interpreter. This allows proxy agents to over-ride the
	/// default interpretation of any message that is received, so as to do
	/// ... something different. Anything different.
	void install_handler(const std::string&, Meth);
};

/** @}*/
} // namespace opencog

#endif // _DISPATCHER_H
