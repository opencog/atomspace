/*
 * opencog/atoms/plan9/SheafHook.cc
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

#include <opencog/atoms/plan9/SheafHook.h>
#include <opencog/atoms/base/Atom.h>

#ifdef HAVE_GUILE
#include <opencog/guile/SchemeEval.h>
#endif

namespace opencog
{
namespace plan9
{

std::vector<std::string> StructuralSheafHook::connectors(const Handle& h) const
{
	std::vector<std::string> out;
	if (nullptr == h)
		return out;

	// Atom::getOutgoingSet() defaults to empty for Nodes, and is
	// overridden by Link to return its actual outgoing set, so this
	// is safe to call uniformly without checking the atom's kind.
	const HandleSeq& oset = h->getOutgoingSet();
	for (size_t i = 0; i < oset.size(); i++)
	{
		out.push_back("+ " + std::to_string(i) + " " +
			oset[i]->to_short_string());
	}
	return out;
}

#ifdef HAVE_GUILE
SchemeSheafHook::SchemeSheafHook(const std::string& scheme_proc_name) :
	_proc_name(scheme_proc_name)
{}

std::vector<std::string> SchemeSheafHook::connectors(const Handle& h) const
{
	std::vector<std::string> out;
	if (nullptr == h)
		return out;

	AtomSpace* as = h->getAtomSpace();
	SchemeEval* eval = SchemeEval::get_scheme_evaluator(as);

	// Call (proc-name (cog-handle h)) via a plain string eval; using
	// the printed atomese form keeps this independent of any single
	// wrapper API for injecting a Handle argument.
	std::string expr = "(" + _proc_name + " " + h->to_short_string() + ")";
	std::string result = eval->eval(expr);

	if (result.empty())
		return out;

	// The Scheme side is expected to print one connector description
	// per line (e.g. via (for-each (lambda (s) (display s) (newline))
	// connector-strings)); split on newlines here.
	size_t start = 0;
	while (start < result.size())
	{
		size_t nl = result.find('\n', start);
		if (nl == std::string::npos)
		{
			std::string line = result.substr(start);
			if (not line.empty()) out.push_back(line);
			break;
		}
		std::string line = result.substr(start, nl - start);
		if (not line.empty()) out.push_back(line);
		start = nl + 1;
	}
	return out;
}
#endif // HAVE_GUILE

} // namespace plan9
} // namespace opencog
