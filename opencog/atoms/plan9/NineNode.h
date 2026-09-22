/*
 * opencog/atoms/plan9/NineNode.h
 *
 * Copyright (c) 2024 OpenCog Foundation
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

#ifndef _OPENCOG_NINE_NODE_H
#define _OPENCOG_NINE_NODE_H

#include <memory>
#include <string>
#include <vector>

#include <opencog/atoms/plan9/Nine9P.h>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

class NineNode;
typedef std::shared_ptr<NineNode> NineNodePtr;

/**
 * Abstract synthetic filesystem node: the unit exposed over the 9P
 * server. Concrete subclasses (see AtomFs.h) map AtomSpace content
 * onto this interface; NineNamespace composes trees of these nodes
 * using Plan 9 bind/mount/union-directory rules, and NineServer
 * drives them from decoded 9P2000 messages.
 *
 * The same underlying atom is always represented by the *same*
 * NineNode instance (see AtomFsIndex::dir_for()), regardless of how
 * many different paths lead to it. This gives every atom a stable,
 * hard-link-like identity across the whole namespace, matching how
 * atoms are already singletons within an AtomSpace.
 */
class NineNode : public std::enable_shared_from_this<NineNode>
{
public:
	virtual ~NineNode() {}

	/// Name of this node, as it appears in its parent directory.
	virtual std::string name() const = 0;

	/// True if this node is a directory.
	virtual bool is_dir() const = 0;

	/// Stable identity of this node (Qid), reused for hard-link-like
	/// aliasing: the same atom reached via different parent paths
	/// resolves to the same, Qid-equal (indeed pointer-equal) node.
	virtual NineQid qid() const = 0;

	/// Total byte length, for plain files; unused for directories.
	virtual uint64_t length() const { return 0; }

	/// List the immediate children of a directory node.
	virtual std::vector<NineNodePtr> children() const { return {}; }

	/// Look up a single named child of a directory node, or nullptr
	/// if there is no such child. The default implementation scans
	/// children(); directory nodes with cheaper direct lookups
	/// should override this.
	virtual NineNodePtr lookup(const std::string& child_name) const;

	/// Read up to `count` bytes starting at `offset` from a file
	/// node. The default implementation is an empty, read-only file.
	virtual std::string read(uint64_t offset, uint32_t count) const
	{
		(void) offset;
		(void) count;
		return std::string();
	}

	/// Write `data` at `offset` into a file node; returns the number
	/// of bytes accepted. The default implementation is read-only.
	virtual uint32_t write(uint64_t offset, const std::string& data)
	{
		(void) offset;
		(void) data;
		return 0;
	}

	/// True if write() may succeed (used to answer Topen mode checks).
	virtual bool is_writable() const { return false; }
};

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // _OPENCOG_NINE_NODE_H
