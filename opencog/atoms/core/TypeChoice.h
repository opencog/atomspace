/*
 * opencog/atoms/core/TypeChoice.h
 *
 * Copyright (C) 2020 Linas Vepstas
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

#ifndef _OPENCOG_TYPE_CHOICE_H
#define _OPENCOG_TYPE_CHOICE_H

#include <opencog/atoms/base/Link.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

typedef std::pair<size_t, size_t> GlobInterval;
class TypeChoice;
typedef std::shared_ptr<TypeChoice> TypeChoicePtr;
typedef std::set<TypeChoicePtr> TypeChoiceSet;

/// The TypeChoice link is used to hold a type description; it is
/// the most general anonymous (un-named) type. It's main usefulness
/// is to hold complex type defintions, and to provide operations
/// on them, such as type-intersection, type-union, filtering and
/// type validation.
///
/// This class implements type-union, so that
///   `(TypeChoice (TypeChoice stuff) (TypeChoice other-stuff))`
/// computes the union of the two. To get type-intersection, use
///   `(TypeIntersection (TypeChoice stuff) (TypeChoice other-stuff))`
///
class TypeChoice : public Link
{
protected:
	TypeSet _simple_typeset;
	HandleSet _deep_typeset;
	TypeChoiceSet _sect_typeset;
	GlobInterval _glob_interval;
	bool _is_untyped;

	void init(bool);
	bool pre_analyze(bool);
	void analyze(Handle);
	void post_analyze(bool);
	GlobInterval make_interval(const HandleSeq&);
	bool is_nonglob_type(const ValuePtr&) const;

	ContentHash compute_hash() const;
public:
	TypeChoice(const HandleSeq&&, Type=TYPE_CHOICE, bool=false);

	TypeChoice(const TypeChoice&) = delete;
	TypeChoice& operator=(const TypeChoice&) = delete;

	TypeSet get_simple_typeset(void) const { return _simple_typeset; }
	HandleSet get_deep_typeset(void) const { return _deep_typeset; }
	GlobInterval get_glob_interval(void) const
		{ return _glob_interval; }

	// The default interval for glob matching.
	static const GlobInterval default_interval(bool);

	bool is_globby(void) const;
	bool is_lower_bound(size_t) const;
	bool is_upper_bound(size_t) const;

	bool is_type(const ValuePtr&) const;
	bool is_type(Type) const;

	bool is_untyped(bool) const;
	bool is_equal(const TypeChoice&) const;

	static Handle factory(const Handle&);
};

typedef std::shared_ptr<TypeChoice> TypeChoicePtr;
static inline TypeChoicePtr TypeChoiceCast(const Handle& h)
	{ return std::dynamic_pointer_cast<TypeChoice>(h); }
static inline TypeChoicePtr TypeChoiceCast(AtomPtr a)
	{ return std::dynamic_pointer_cast<TypeChoice>(a); }

#define createTypeChoice std::make_shared<TypeChoice>

/** @}*/
}

#endif // _OPENCOG_TYPE_CHOICE_H
