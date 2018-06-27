/*
 * PatternTerm.cc
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 *
 * Created by Nil Geisweiller Oct 2016
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

#include "PatternTerm.h"

namespace opencog {

PatternTerm::PatternTerm()
	: _handle(Handle::UNDEFINED), _parent(PatternTerm::UNDEFINED),
	  _has_any_bound_var(false)
{}

PatternTerm::PatternTerm(const PatternTermPtr& parent, const Handle& h)
	: _handle(h), _parent(parent),
	  _quotation(parent->_quotation.level(),
	             false /* necessarily false since it is local */),
	  _has_any_bound_var(false)
{
	Type t = h->get_type();

	// Discard the following QuoteLink, UnquoteLink or LocalQuoteLink
	// as it is serving its quoting or unquoting function.
	if (_quotation.consumable(t)) {
		if (1 != h->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink/LocalQuoteLink has "
			                            "unexpected arity!");
		_handle = h->getOutgoingAtom(0);
	}

	// Update the quotation state
	_quotation.update(t);
}

void PatternTerm::addOutgoingTerm(const PatternTermPtr& ptm)
{
	_outgoing.push_back(ptm);
}

Handle PatternTerm::getHandle()
{
	return _handle;
}

PatternTermPtr PatternTerm::getParent()
{
	return _parent;
}

PatternTermSeq PatternTerm::getOutgoingSet() const
{
	PatternTermSeq oset;
	for (PatternTermWPtr w : _outgoing)
	{
		PatternTermPtr s(w.lock());
		if (s) oset.push_back(s);
	}

	return oset;
}

Arity PatternTerm::getArity() const
{
	return _outgoing.size();
}

Quotation& PatternTerm::getQuotation()
{
	return _quotation;
}

const Quotation& PatternTerm::getQuotation() const
{
	return _quotation;
}

bool PatternTerm::isQuoted() const
{
	return _quotation.is_quoted();
}

bool PatternTerm::hasAnyBoundVariable() const
{
	return _has_any_bound_var;
}

PatternTermPtr PatternTerm::getOutgoingTerm(Arity pos) const
{
	// Checks for a valid position
	if (pos < getArity()) {
		PatternTermPtr s(_outgoing[pos].lock());
		if (not s)
			throw RuntimeException(TRACE_INFO,
			                       "expired outgoing set index %d", pos);
		return s;
	} else {
		throw RuntimeException(TRACE_INFO,
		                       "invalid outgoing set index %d", pos);
	}
}

void PatternTerm::addBoundVariable()
{
	if (!_has_any_bound_var)
	{
		_has_any_bound_var = true;
		if (_parent != PatternTerm::UNDEFINED)
			_parent->addBoundVariable();
	}
}

std::string PatternTerm::to_string() const { return to_string(":"); }

std::string PatternTerm::to_string(const std::string& indent) const
{
	if (not _handle) return "-";
	std::string str = _parent->to_string();
	str += indent + std::to_string(_handle.value());
	return str;
}

std::string oc_to_string(const PatternTerm& pt, const std::string& indent)
{
	return pt.to_string(indent);
}

std::string oc_to_string(const PatternTermPtr& pt_ptr, const std::string& indent)
{
	return pt_ptr->to_string();
}

} // ~namespace opencog
