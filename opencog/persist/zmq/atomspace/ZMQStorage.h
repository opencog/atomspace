/*
 * FUNCTION:
 * Base class for ZeroMQ-backed persistent storage.
 *
 * HISTORY:
 * Copyright (c) 2015 Hendy Irawan <ceefour666@gmail.com>
 *
 * LICENSE:
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

#ifndef _OPENCOG_PERSISTENT_ZMQ_STORAGE_H
#define _OPENCOG_PERSISTENT_ZMQ_STORAGE_H

#include <atomic>
#include <mutex>
#include <set>
#include <thread>
#include <vector>

#include <opencog/util/async_method_caller.h>
#include <opencog/atomspace/Atom.h>
#include <opencog/atomspace/Link.h>
#include <opencog/atomspace/Node.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/types.h>

namespace opencog
{
/** \addtogroup grp_persist
 *  @{
 */

class ZMQStorage
{
	private:

	public:
		ZMQStorage();
		~ZMQStorage();
};

/** @}*/
} // namespace opencog

#endif // _OPENCOG_PERSISTENT_ZMQ_STORAGE_H
