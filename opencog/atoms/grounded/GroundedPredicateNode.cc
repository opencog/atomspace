/*
 * opencog/atoms/grounded/GroundedPredicateNode.cc
 *
 * Copyright (C) 2009, 2013, 2014, 2015, 2020 Linas Vepstas
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

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/core/DefineLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/execution/Force.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atomspace/AtomSpace.h>

#include <opencog/atoms/grounded/GroundedPredicateNode.h>
#include "LibraryRunner.h"
#include "PythonRunner.h"
#include "SCMRunner.h"


using namespace opencog;

GroundedPredicateNode::GroundedPredicateNode(std::string s)
	: GroundedProcedureNode(GROUNDED_PREDICATE_NODE, std::move(s))
{
	init();
}

GroundedPredicateNode::GroundedPredicateNode(Type t, std::string s)
	: GroundedProcedureNode(t, std::move(s))
{
	if (not nameserver().isA(t, GROUNDED_PREDICATE_NODE))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a GroundedProcedureNode, got %s", tname.c_str());
	}
	init();
}

GroundedPredicateNode::~GroundedPredicateNode()
{
	if (_runner) delete _runner;
}

void GroundedPredicateNode::init()
{
	_eager = false;
	_runner = nullptr;

	// Get the schema name.
	const std::string& schema = get_name();

	// At this point, we only run scheme and python schemas.
	if (0 == schema.compare(0, 4, "scm:", 4))
	{
#ifdef HAVE_GUILE
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 4;
		while (' ' == schema[pos]) pos++;
		_runner = new SCMRunner(schema.substr(pos));
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have guile support in it; "
			"Cannot evaluate scheme GroundedPredicateNode!");
#endif /* HAVE_GUILE */
		return;
	}

	if (0 == schema.compare(0, 10, "scm-eager:", 10))
	{
#ifdef HAVE_GUILE
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 10;
		_eager = true;
		while (' ' == schema[pos]) pos++;
		_runner = new SCMRunner(schema.substr(pos));
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have guile support in it; "
			"Cannot evaluate scheme GroundedPredicateNode!");
#endif /* HAVE_GUILE */
		return;
	}

	if (0 == schema.compare(0, 3, "py:", 3))
	{
#ifdef HAVE_CYTHON
		// Be friendly, and strip leading white-space, if any.
		size_t pos = 3;
		while (' ' == schema[pos]) pos++;
		_runner = new PythonRunner(schema.substr(pos));
#else
		throw RuntimeException(TRACE_INFO,
			"This binary does not have python support in it; "
			"Cannot evaluate python GroundedPredicateNode!");
#endif /* HAVE_CYTHON */
		return;
	}

	if (0 == schema.compare(0, 4, "lib:", 4))
	{
		_runner = new LibraryRunner(schema);
		return;
	}
}

// ----------------------------------------------------------

/// `execute_args()` -- evaluate a GroundedPredicateNode with arguments.
///
/// Expects "args" to be a ListLink. These arguments will be
/// substituted into the predicate. Then the predicate as a whole
/// will be evaluated.
///
ValuePtr GroundedPredicateNode::execute_args(AtomSpace* as,
                                             const ValuePtr& cargs,
                                             bool silent)
{
	// Perform "eager evaluation" instead of "lazy evaluation".
	if (_eager and _runner)
	{
		Handle exargs(force_execute(as, HandleCast(cargs), silent));
		return _runner->evaluate(as, exargs, silent);
	}

	if (_runner) return _runner->evaluate(as, cargs, silent);

	// Unknown procedure type.
	throw RuntimeException(TRACE_INFO,
	     "Cannot evaluate unknown GroundedPredicateNode: %s",
	      get_name().c_str());
}

DEFINE_NODE_FACTORY(GroundedPredicateNode, GROUNDED_PREDICATE_NODE)
