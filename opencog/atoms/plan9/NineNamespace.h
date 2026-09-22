/*
 * opencog/atoms/plan9/NineNamespace.h
 *
 * A per-process Plan 9 style namespace: a table mapping namespace
 * paths to ordered stacks of NineNode trees ("union directories"),
 * built up via bind()/mount(), and resolved with walk(). See
 * Pike & Presotto et al., "Plan 9 from Bell Labs", and the bind(1)
 * / mount(1) / intro(5) manual pages for the semantics this models.
 *
 * Each 9P connection/process (see NineServer) owns an independent
 * NineNamespace, typically obtained by copy-constructing from a
 * parent's namespace -- mirroring Plan 9's rfork(RFNAMEG): the child
 * starts with the same view but subsequent binds in either the
 * parent or the child do not affect the other. This is what gives
 * every atom its own *per-process* namespace entry, layered over
 * whatever is shared.
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

#ifndef _OPENCOG_NINE_NAMESPACE_H
#define _OPENCOG_NINE_NAMESPACE_H

#include <map>
#include <string>
#include <vector>

#include <opencog/atoms/plan9/NineNode.h>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// Union-directory ordering, as in Plan 9's bind(1)/mount(1) -M/-b/-a
/// flags: MREPL replaces whatever was there, MBEFORE puts the new
/// tree earlier (shadowing it), MAFTER puts it later (shadowed by it).
enum NineMountOrder { MREPL, MBEFORE, MAFTER };

class NineNamespace
{
public:
	explicit NineNamespace(NineNodePtr root);
	NineNamespace(const NineNamespace&) = default;
	NineNamespace& operator=(const NineNamespace&) = default;

	/// Attach `device` at `at_path`, unioned per `order` with
	/// whatever is already visible there.
	void mount(const std::string& at_path, NineNodePtr device,
	           NineMountOrder order = MREPL);

	/// Bind whatever currently resolves at `old_path` onto
	/// `new_path` as well (Plan 9 `bind old_path new_path`).
	/// Throws std::runtime_error if `old_path` does not resolve.
	void bind(const std::string& new_path, const std::string& old_path,
	          NineMountOrder order = MREPL);

	/// Remove any override previously installed (by mount()/bind())
	/// at exactly `at_path`, reverting it to whatever the underlying
	/// static tree provides.
	void unmount(const std::string& at_path);

	/// Resolve `path` to the node that should be used for
	/// non-directory operations: the first (shadowing) entry of the
	/// union stack, or nullptr if `path` does not resolve.
	NineNodePtr walk(const std::string& path) const;

	/// Resolve `path` to its full union stack (outermost/shadowing
	/// entry first), for directory-listing purposes. Empty if `path`
	/// does not resolve at all.
	std::vector<NineNodePtr> walk_union(const std::string& path) const;

	/// The union of all (name-deduplicated, first-occurrence-wins)
	/// children visible at `path`, honoring the same shadow order as
	/// walk_union().
	std::vector<NineNodePtr> list(const std::string& path) const;

	/// Canonicalize a path: collapse repeated/trailing slashes,
	/// always returning a leading "/" (root is "/").
	static std::string canon(const std::string& path);

private:
	// Keyed by canonical path; each entry is the union stack that
	// should be used starting at (and below, until another override
	// is met) that exact path.
	std::map<std::string, std::vector<NineNodePtr>> _mounts;

	static std::vector<std::string> split(const std::string& path);
};

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // _OPENCOG_NINE_NAMESPACE_H
