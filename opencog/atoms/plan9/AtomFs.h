/*
 * opencog/atoms/plan9/AtomFs.h
 *
 * Maps an AtomSpace onto a synthetic Plan 9 style file tree:
 *
 *   /by-type/<TYPE_NAME>/<id>/          -- one directory per atom
 *   /by-handle/<id>/                    -- the same directories, by id
 *   /new                                -- control file: write Atomese
 *                                          (Scheme) text to create an
 *                                          atom, then read back its path
 *
 * Per atom directory <id>/ contains:
 *   name                                -- Node name (empty for Links)
 *   type                                -- type name
 *   to_string                           -- full to_string() rendering
 *   tv                                  -- convenience view of the
 *                                          truth-value key, if any
 *   outgoing/0, outgoing/1, ...         -- hard-links to each element
 *                                          of the outgoing set, in order
 *   incoming/<id>, ...                  -- hard-links to each element
 *                                          of the incoming set
 *   values/<id>                         -- one file per (key . value)
 *                                          pair attached to the atom
 *   sheaf/connectors, sheaf/count       -- present only if a SheafHook
 *                                          is configured and reports at
 *                                          least one connector
 *
 * `<id>` is the atom's content hash (Handle::value()), rendered as a
 * fixed-width hex string, so it is stable for a given atom's content
 * and safe to use as a path component.
 *
 * Every atom maps to exactly one, shared NineNode instance (see
 * AtomFsIndex::dir_for()), no matter how many different paths lead
 * to it -- outgoing/incoming "links" are the *same* object as the
 * corresponding by-handle/<id> entry, giving genuine hard-link
 * semantics (identical Qids) as in a real Plan 9 namespace.
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

#ifndef _OPENCOG_ATOM_FS_H
#define _OPENCOG_ATOM_FS_H

#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/plan9/NineNode.h>
#include <opencog/atoms/plan9/SheafHook.h>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Owns the mapping from an AtomSpace's atoms to their (memoized)
 * NineNode representations, and builds the top-level directory
 * tree (by-type/, by-handle/, new) for that AtomSpace.
 *
 * An AtomFsIndex must outlive every NineNode it has handed out, and
 * outlive any NineNamespace/NineServer built on top of its root().
 */
class AtomFsIndex
{
public:
	explicit AtomFsIndex(const AtomSpacePtr& as,
	                      SheafHookPtr hook = nullptr);

	/// The root directory node ("/") for this AtomSpace.
	NineNodePtr root() const { return _root; }

	/// Get-or-create the shared directory node for `h`.
	NineNodePtr dir_for(const Handle& h);

	const AtomSpacePtr& atomspace() const { return _as; }
	const SheafHookPtr& sheaf_hook() const { return _hook; }

	/// Stable, filename-safe identifier for a Handle: its content
	/// hash, rendered as fixed-width lower-case hex.
	static std::string id_of(const Handle& h);

	/// Inverse of id_of(): true and sets `h` if `id` names a Handle
	/// this index has already produced a directory for.
	bool handle_for_id(const std::string& id, Handle& h) const;

private:
	AtomSpacePtr _as;
	SheafHookPtr _hook;
	std::mutex _mtx;
	std::unordered_map<ContentHash, NineNodePtr> _cache;
	std::unordered_map<std::string, Handle> _ids;
	NineNodePtr _root;
};

typedef std::shared_ptr<AtomFsIndex> AtomFsIndexPtr;

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // _OPENCOG_ATOM_FS_H
