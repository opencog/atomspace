/*
 * TransposeColumn.cc
 *
 * Copyright (C) 2015, 2022, 2025 Linas Vepstas
 *
 * Author: Linas Vepstas <linasvepstas@gmail.com>  January 2009
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the
 * exceptions at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public
 * License along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <opencog/atoms/core/FunctionLink.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/FloatValue.h>

#include "TransposeColumn.h"

using namespace opencog;

TransposeColumn::TransposeColumn(const HandleSeq&& oset, Type t)
	: Link(std::move(oset), t)
{
	if (not nameserver().isA(t, TRANSPOSE_COLUMN))
	{
		const std::string& tname = nameserver().getTypeName(t);
		throw InvalidParamException(TRACE_INFO,
			"Expecting a TransposeColumn, got %s", tname.c_str());
	}
}

// ---------------------------------------------------------------

/// Return a FloatValue vector.
ValuePtr TransposeColumn::do_handle_loop(AtomSpace* as, bool silent,
                                         const HandleSeq& hseq)
{
	// Upon entry, we don't yet know how many columns we will need.
	// Assume that all rows will be the same length, so that the
	// first row will provide the right size.
	size_t ncols = 0;
	std::vector<ValuePtr> vcols;
	for (const Handle& h : hseq)
	{
		ValuePtr vp(FunctionLink::get_value(as, silent, h));

		if (0 == ncols)
		{
			ncols = vp->size();
			vcols.reserve(ncols);

			if (vp->is_type(FLOAT_VALUE))
			{
				const std::vector<double>& vals = FloatValueCast(vp)->value();
				for (size_t i=0; i< ncols; i++)
					vcols[i] = createFloatValue(vals[i]);
			}
			else if (vp->is_type(NUMBER_NODE))
			{
				const std::vector<double>& vals = NumberNodeCast(vp)->value();
				for (size_t i=0; i< ncols; i++)
					vcols[i] = createFloatValue(vals[i]);
			}
			else
				throw RuntimeException(TRACE_INFO,
					"I don't know what to do here\n");

			continue;
		}
		if (vp->is_type(FLOAT_VALUE))
		{
			const std::vector<double>& vals = FloatValueCast(vp)->value();
			for (size_t i=0; i< ncols; i++)
				FloatValueCast(vcols[i]) -> _value.push_back(vals[i]);
		}
		else if (vp->is_type(NUMBER_NODE))
		{
			const std::vector<double>& vals = NumberNodeCast(vp)->value();
			for (size_t i=0; i< ncols; i++)
				FloatValueCast(vcols[i]) -> _value.push_back(vals[i]);
		}
	}

	return createLinkValue(std::move(vcols));
}

// ---------------------------------------------------------------

/// Return a FloatValue vector.
ValuePtr TransposeColumn::do_execute(AtomSpace* as, bool silent)
{
	// If the given Atom is executable, then execute it.
	Handle base(_outgoing[0]);
	if (base->is_executable())
	{
		ValuePtr vpe(base->execute(as, silent));
		if (vpe->is_atom())
			base = HandleCast(vpe);
		else
		{
			if (vpe->is_type(FLOAT_VALUE))
				return vpe;

			if (not vpe->is_type(LINK_VALUE))
				throw RuntimeException(TRACE_INFO,
					"Expecting LinkValue, got %s\n",
					vpe->to_string());

			// If we are here, we've got a LinkValue.
			// Inside of loop is cut-n-paste of that below.
			std::vector<double> dvec;
			dvec.reserve(vpe->size());
			for (const ValuePtr& v : LinkValueCast(vpe)->value())
			{
				// FunctionLink::get_value() tries to execute
				// the value, if it's executable. Is that overkill,
				// or is that needed? When would a vector of functions
				// arise?
				ValuePtr vp(FunctionLink::get_value(as, silent, v));

				// Expecting exactly one float per item. That's because
				// I don't know what it means if there is more than one,
				// flattening seems like the wrong thing to do.
				if (1 != vp->size())
					throw RuntimeException(TRACE_INFO,
						"Expecting exactly one number per item, got %lu\n",
						vp->size());

				if (vp->is_type(FLOAT_VALUE))
					dvec.push_back(FloatValueCast(vp)->value()[0]);
				else if (vp->is_type(NUMBER_NODE))
					dvec.push_back(NumberNodeCast(vp)->get_value());
				else
					throw RuntimeException(TRACE_INFO,
						"Expecting numeric value, got %s\n",
						vp->to_string());
			}
			return createFloatValue(std::move(dvec));
		}
	}

	// If we are here, then base is an atom.
	if (base->is_type(NUMBER_NODE))
	{
		std::vector<double> nums(NumberNodeCast(base)->value());
		return createFloatValue(std::move(nums));
	}

	// If we are here, then base is an link. Expect
	// it to contain things that evaluate to a double
	return do_handle_loop(as, silent, base->getOutgoingSet());
}

// ---------------------------------------------------------------

/// Return a FloatValue vector.
ValuePtr TransposeColumn::execute(AtomSpace* as, bool silent)
{
	if (1 == _outgoing.size())
		return do_execute(as, silent);

	return do_handle_loop(as, silent, _outgoing);
}

DEFINE_LINK_FACTORY(TransposeColumn, TRANSPOSE_COLUMN)

/* ===================== END OF FILE ===================== */
