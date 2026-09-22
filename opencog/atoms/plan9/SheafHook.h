/*
 * opencog/atoms/plan9/SheafHook.h
 *
 * Pluggable "sheaf" extension for the Plan 9 AtomSpace namespace.
 *
 * There is no current, maintained C++ implementation of sheaf
 * theory/connector decomposition in this repository -- see
 * opencog/sheaf/README.md, which states that the real
 * implementation lives in the external opencog/learn project (as
 * mst-parser.scm/sheaf.scm, etc), implemented in Scheme. Rather
 * than re-implement (or worse, fork) that logic in C++, this header
 * defines a small abstract interface that the AtomFs synthetic
 * filesystem uses to render a "sheaf/" directory per atom, plus:
 *
 *   - StructuralSheafHook: a dependency-free fallback that treats a
 *     Link's own outgoing set as its connectors. This is a
 *     defensible minimal structural view, *not* a claim to
 *     replicate the linguistic/sheaf-theoretic connector calculus.
 *
 *   - SchemeSheafHook (HAVE_GUILE only): calls a user-named Scheme
 *     procedure, via the existing SchemeEval bridge, so that a
 *     caller can wire this up to the real sheaf.scm (or
 *     opencog/learn's successor) without this module needing to
 *     depend on it directly.
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

#ifndef _OPENCOG_NINE_SHEAF_HOOK_H
#define _OPENCOG_NINE_SHEAF_HOOK_H

#include <memory>
#include <string>
#include <vector>

#include <opencog/atoms/base/Handle.h>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Abstract provider of "connector" descriptions for an atom, used
 * to populate the synthetic sheaf/ directory of AtomFs.
 */
class SheafHook
{
public:
	virtual ~SheafHook() {}

	/// Return one human-readable line of text per connector of `h`.
	/// An empty result means "no sheaf structure for this atom" and
	/// the sheaf/ directory is simply omitted for it.
	virtual std::vector<std::string> connectors(const Handle& h) const = 0;
};

typedef std::shared_ptr<SheafHook> SheafHookPtr;

/**
 * Minimal, dependency-free fallback hook: for a Link, each element
 * of its outgoing set is reported as one connector (direction "+",
 * arbitrary/unordered), described by that element's own short
 * string form. Nodes have no connectors under this scheme.
 */
class StructuralSheafHook : public SheafHook
{
public:
	std::vector<std::string> connectors(const Handle& h) const override;
};

#ifdef HAVE_GUILE
/**
 * Bridges to a Scheme procedure (looked up by name, called with the
 * atom as its sole argument) via the existing SchemeEval evaluator.
 * The Scheme procedure is expected to return a list of strings; any
 * other return value yields zero connectors. This is the intended
 * way to plug in a real sheaf/connector implementation, such as one
 * derived from opencog/sheaf/attic or opencog/learn's sheaf.scm,
 * without this module depending on it at build time.
 */
class SchemeSheafHook : public SheafHook
{
public:
	explicit SchemeSheafHook(const std::string& scheme_proc_name);
	std::vector<std::string> connectors(const Handle& h) const override;

private:
	std::string _proc_name;
};
#endif // HAVE_GUILE

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // _OPENCOG_NINE_SHEAF_HOOK_H
