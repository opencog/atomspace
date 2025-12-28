/*
 * Instantiator.h
 *
 * Copyright (C) 2009, 2014, 2025 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
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
 */

#ifndef _OPENCOG_INSTANTIATOR_H
#define _OPENCOG_INSTANTIATOR_H

#include <opencog/atomspace/AtomSpace.h>

namespace opencog {

ValuePtr instantiate(AtomSpace*,
                     const GroundingMap&,
                     const Handle& expr,
                     bool silent=false);

} // namespace opencog

#endif // _OPENCOG_INSTANTIATOR_H

