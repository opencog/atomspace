/*
 * PatternTerm.cc
 *
 * Copyright (C) 2016 OpenCog Foundation
 * All Rights Reserved
 *
 * Created by Nil Geisweiller Oct 2016
 * SPDX-License-Identifier: AGPL-3.0-or-later
 */

#include <opencog/util/oc_assert.h>
#include "PatternTerm.h"

namespace opencog {

const PatternTermPtr PatternTerm::UNDEFINED(std::make_shared<PatternTerm>());

PatternTerm::PatternTerm(void)
	: _handle(Handle::UNDEFINED),
	  _quote(Handle::UNDEFINED),
	  _parent(PatternTerm::UNDEFINED),
	  _has_any_bound_var(false),
	  _has_bound_var(false),
	  _is_bound_var(false),
	  _has_any_globby_var(false),
	  _has_globby_var(false),
	  _is_globby_var(false),
	  _has_any_evaluatable(false),
	  _has_evaluatable(false),
	  _is_virtual(false),
	  _has_any_unordered_link(false),
	  _is_literal(false),
	  _is_present(false),
	  _is_absent(false),
	  _is_choice(false),
	  _is_always(false)
{}

PatternTerm::PatternTerm(const PatternTermPtr& parent, const Handle& h)
	: _handle(h), _quote(Handle::UNDEFINED), _parent(parent),
	  _quotation(parent->_quotation.level(),
	             false /* necessarily false since it is local */),
	  _has_any_bound_var(false),
	  _has_bound_var(false),
	  _is_bound_var(false),
	  _has_any_globby_var(false),
	  _has_globby_var(false),
	  _is_globby_var(false),
	  _has_any_evaluatable(false),
	  _has_evaluatable(false),
	  _is_virtual(false),
	  _has_any_unordered_link(false),
	  _is_literal(false),
	  _is_present(false),
	  _is_absent(false),
	  _is_choice(false),
	  _is_always(false)
{
	Type t = h->get_type();

	// Discard the following QuoteLink, UnquoteLink or LocalQuoteLink
	// as it is serving its quoting or unquoting function.
	if (_quotation.consumable(t)) {
		if (1 != h->get_arity())
			throw InvalidParamException(TRACE_INFO,
			                            "QuoteLink/UnquoteLink/LocalQuoteLink has "
			                            "unexpected arity!");
		// Save the quotes, useful for mapping patterns to grounds
		_quote = _handle;
		_handle = h->getOutgoingAtom(0);
	}

	// Update the quotation state
	_quotation.update(t);
}

PatternTermPtr PatternTerm::addOutgoingTerm(const Handle& h)
{
	PatternTermPtr ptm(createPatternTerm(shared_from_this(), h));
	_outgoing.push_back(ptm);
	return ptm;
}

PatternTermSeq PatternTerm::getOutgoingSet() const
{
	PatternTermSeq oset;
	for (const PatternTermWPtr& w : _outgoing)
	{
		PatternTermPtr s(w.lock());
		OC_ASSERT(nullptr != s, "Unexpected corruption of PatternTerm oset!");
		oset.push_back(s);
	}

	return oset;
}

PatternTermPtr PatternTerm::getOutgoingTerm(Arity pos) const
{
	// Checks for a valid position
	if (pos < getArity()) {
		PatternTermPtr s(_outgoing[pos].lock());
		OC_ASSERT(nullptr != s, "Unexpected missing PatternTerm oset entry!");
		return s;
	} else {
		throw RuntimeException(TRACE_INFO,
		                       "invalid outgoing set index %d", pos);
	}
}

/**
 * isDescendant - return true if `this` is a lineal descendant of `ptm`.
 * That is, return true if `ptm` appears as a parent somewhere in the
 * chain of parents of `this`.
 *
 * Mnemonic device: child->isDescendant(parent) == true
 */
bool PatternTerm::isDescendant(const PatternTermPtr& ptm) const
{
	if (PatternTerm::UNDEFINED == _parent) return false;
	if (_parent == ptm) return true;
	if (*_parent == *ptm) return true;
	return _parent->isDescendant(ptm);
}

// ==============================================================
/**
 * Equality operator.  Both the content must match, and the path
 * taken to get to the content must match.
 */
bool PatternTerm::operator==(const PatternTerm& other)
{
	if (_handle != other._handle) return false;
	if (_parent != other._parent) return false;
	if (PatternTerm::UNDEFINED == _parent) return true;

	return _parent->operator==(*other._parent);
}

// ==============================================================

// Mark recursively, all the way to the root.
void PatternTerm::addAnyBoundVar()
{
	if (not _has_any_bound_var)
	{
		_has_any_bound_var = true;
		if (_parent->_handle)
			_parent->addAnyBoundVar();
	}
}

/// Set two flags: one flag (the "any" flag) is set recursively from
/// a variable, all the way up to the root, indicating that there's
/// a variable on this path.  The other flag gets set only on the
/// variable, and it's immediate parent (i.e. the holder of the
/// variable).
void PatternTerm::addBoundVariable()
{
	if (isQuoted()) return;

	// Mark just this term (the variable itself)
	// and mark the term that holds us.
	_is_bound_var = true;
	if (_parent->_handle)
			_parent->_has_bound_var = true;

	// Mark recursively, all the way to the root.
	addAnyBoundVar();
}

// ==============================================================
// Just like above, but for globs.

void PatternTerm::addAnyGlobbyVar()
{
	if (not _has_any_globby_var)
	{
		_has_any_globby_var = true;
		if (_parent->_handle)
			_parent->addAnyGlobbyVar();
	}
}

void PatternTerm::addGlobbyVar()
{
	if (isQuoted()) return;

	_is_globby_var = true;

	if (_parent->_handle)
		_parent->_has_globby_var = true;

	addAnyGlobbyVar();
}


// ==============================================================
// Just like above, but for evaluatables.

void PatternTerm::addAnyEvaluatable()
{
	if (_has_any_evaluatable) return;

	_has_any_evaluatable = true;
	if (_parent->_handle)
		_parent->addAnyEvaluatable();
}

void PatternTerm::addEvaluatable()
{
	_has_evaluatable = true;

	if (_parent->_handle)
		_parent->_has_evaluatable = true;

	addAnyEvaluatable();
}

// ==============================================================

void PatternTerm::markVirtual()
{
	// If quoted, it cannot be evaluated.
	if (isQuoted()) return;

	_is_virtual = true;
}

// ==============================================================

void PatternTerm::addUnorderedLink()
{
	if (_has_any_unordered_link) return;

	_has_any_unordered_link = true;
	if (_parent->_handle)
		_parent->addUnorderedLink();
}

// ==============================================================

void PatternTerm::markLiteral()
{
	if (_is_literal) return;

	_is_literal = true;
	_has_evaluatable = false;
	_has_any_evaluatable = false;
	for (PatternTermPtr& ptm : getOutgoingSet())
		ptm->markLiteral();
}

// ==============================================================

void PatternTerm::markPresent()
{
	// If its literal, its effectively quoted, so cannot be present.
	if (_is_literal or isQuoted()) return;

	_is_present = true;

	// By definition, everything underneath is literal
	for (PatternTermPtr& ptm : getOutgoingSet())
		ptm->markLiteral();
}

// ==============================================================

void PatternTerm::markAbsent()
{
	// If quoted, it cannot be interpreted as absent.
	if (isQuoted()) return;

	_is_absent = true;
}

// ==============================================================

void PatternTerm::markChoice()
{
	// If its literal, its effectively quoted, so cannot be a choice.
	if (_is_literal or isQuoted()) return;

	_is_choice = true;

	// By definition, everything underneath is present, or literal
	for (PatternTermPtr& ptm : getOutgoingSet())
	{
		if (not ptm->isPresent()) ptm->markLiteral();
	}
}

// ==============================================================

void PatternTerm::markAlways()
{
	// If its quoted, it cannot be always.
	if (isQuoted()) return;

	_is_always = true;
}

// ==============================================================

std::string PatternTerm::to_short_string() const { return to_string(": "); }

std::string PatternTerm::to_short_string(const std::string& sep) const
{
	// Term is null-terminated at the top.
	// Top term never has a handle in it.
	if (not _handle) return "-";
	std::string str = _parent->to_short_string(sep);
	str += sep + _handle->id_to_string();
	return str;
}

std::string PatternTerm::flag_string() const
{
	std::string str;
	if (isQuoted()) str += "Q: ";
	if (_has_any_bound_var) str += "HABV: ";
	if (_has_bound_var) str += "HBV: ";
	if (_is_bound_var) str += "BV: ";
	if (_has_any_globby_var) str += "HAGV: ";
	if (_has_globby_var) str += "HGV: ";
	if (_is_globby_var) str += "GV: ";
	if (_has_any_evaluatable) str += "EE: ";
	if (_has_evaluatable) str += "E: ";
	if (_is_virtual) str += "V: ";
	if (_has_any_unordered_link) str += "U: ";
	if (_is_literal) str += "L: ";
	if (_is_present) str += "P: ";
	if (_is_absent) str += "A: ";
	if (_is_choice) str += "C: ";
	if (_is_always) str += "AW: ";
	str += _handle->id_to_string();
	return str;
}

std::string PatternTerm::to_string() const { return to_string(""); }

std::string PatternTerm::to_string(const std::string& indent) const
{
	// Term is null-terminated at the top.
	// Top term never has a handle in it.
	if (not _handle) return "\n";
	std::string str = _parent->to_string(indent + "   ");
	str += indent;
	str += nameserver().getTypeName(getQuote()->get_type()) + " : ";
	str += flag_string() + "\n";
	return str;
}

std::string PatternTerm::to_full_string() const { return to_full_string(""); }

std::string PatternTerm::to_full_string(const std::string& indent) const
{
	if (getQuote()->is_node())
	{
		std::string str = getQuote()->to_short_string(indent);
		str += "\t; " + flag_string() + "\n";
		return str;
	}

	std::string str = indent;
	std::string more_indent = indent + "  "; // two spaces
	str += "(" + nameserver().getTypeName(getQuote()->get_type());
	str += "\t\t; " + flag_string() + "\n";
	for (const PatternTermPtr& ptm: getOutgoingSet())
	{
		if (str.back() == ')') str += "\n";
		str += ptm->to_full_string(more_indent);
	}
	if (str.back() != ')') str += indent;
	str += ")";
	return str;
}

std::string oc_to_string(const PatternTerm& pt, const std::string& indent)
{
	return pt.to_string(indent);
}

std::string oc_to_string(const PatternTermPtr& ptr, const std::string& indent)
{
	return ptr->to_string();
}

std::string oc_to_string(const PatternTermSeq& pts, const std::string& indent)
{
	std::string str;
	size_t i=0;
	str += "PatternTermSeq has " + std::to_string(pts.size()) + " terms\n";
	for (const PatternTermPtr& ptm : pts)
	{
		str += indent + "term[" + std::to_string(i) + "]:\n";
		str += ptm->to_full_string(indent + "  ") + "\n";
		i++;
	}
	return str;
}

} // ~namespace opencog
