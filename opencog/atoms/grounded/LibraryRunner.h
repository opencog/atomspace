/*
 * opencog/atoms/grounded/LibraryRunner.h
 *
 * Copyright (C) 2020 Linas Vepstas
 * SPDX-License-Identifier: AGPL-3.0-or-later
 * All Rights Reserved
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

#ifndef _OPENCOG_LIBRARY_RUNNER_H
#define _OPENCOG_LIBRARY_RUNNER_H

#include <string>
#include <opencog/atoms/grounded/Runner.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// Generic shared-library foreign-function interface.
/// Currently used only by the Haskell bindings.
class LibraryRunner : public Runner
{
	std::string _fname;
	void* sym;

public:
	LibraryRunner(const std::string);
	LibraryRunner(const LibraryRunner&) = delete;
	LibraryRunner& operator=(const LibraryRunner&) = delete;

	virtual ValuePtr execute(AtomSpace*, const ValuePtr&, bool=false);
	virtual ValuePtr evaluate(AtomSpace*, const ValuePtr&, bool=false);
};

/** @}*/
}

#endif // _OPENCOG_LIBRARY_RUNNER_H
