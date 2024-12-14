/*
 * opencog/cython/Utilities.h
 *
 * Copyright (C) 2011 by The OpenCog Foundation
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

#ifndef _OPENCOG_UTILITIES_H
#define _OPENCOG_UTILITIES_H
#include "opencog/atoms/base/Handle.h"
#include "opencog/atomspace/AtomSpace.h"

namespace opencog {

void initialize_python();
void finalize_python();
Handle add_node(Type, std::string);
Handle add_link(Type, HandleSeq);
AtomSpacePtr get_context_atomspace(void);

} // namespace opencog


#endif // _OPENCOG_UTILITIES_H
