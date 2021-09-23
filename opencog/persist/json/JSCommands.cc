/*
 * JSCommands.cc
 * Fast command interpreter for basic JSON AtomSpace commands.
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

#include <time.h>

#include <functional>
#include <iomanip>
#include <string>

#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include "JSCommands.h"
#include "Json.h"

using namespace opencog;

static std::string reterr(const std::string& cmd)
{
	std::string err =
		"JSON/JavaScript function not supported: >>" + cmd + "<<\n";
	return err;
}

/// The cogserver provides a network API to send/receive Atoms, encoded
/// as JSON, over the internet. This is NOT as efficient as the
/// s-expression API, but is more convenient for web developers.
//
std::string JSCommands::interpret_command(AtomSpace* as,
                                          const std::string& cmd)
{
	// Fast dispatch. There should be zero hash collisions
	// here. If there are, we are in trouble. (Well, if there
	// are collisions, just prepend a dot?)
	static const size_t gtatm = std::hash<std::string>{}("getAtoms");

	// Ignore comments, blank lines
	if ('/' == cmd[0]) return "";
	if ('\n' == cmd[0]) return "";

	// Find the command and dispatch
	size_t cpos = cmd.find_first_of(".");
	if (std::string::npos == cpos) return reterr(cmd);

	size_t pos = cmd.find_first_not_of(". \n\t", cpos);
	if (std::string::npos == pos) return reterr(cmd);

	size_t epos = cmd.find_first_of("( \n\t", pos);
	if (std::string::npos == epos) return reterr(cmd);

	size_t act = std::hash<std::string>{}(cmd.substr(pos, epos-pos));

	// -----------------------------------------------
	// AtomSpace.getAtoms("Node", true)
	if (gtatm == act)
	{
printf("yo: %s\n", cmd.substr(pos, epos-pos).c_str());
return "hello world";
	}

	return reterr(cmd);
}
