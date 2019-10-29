/*
 * opencog/atoms/truthvalue/TruthValue.h
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Guilherme Lamacie
 *            Welter Silva <welter@vettalabs.com>
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

#ifndef _OPENCOG_TRUTH_VALUE_H
#define _OPENCOG_TRUTH_VALUE_H

#include <memory>
#include <string>
#include <vector>

#include <opencog/util/empty_string.h>
#include <opencog/util/exceptions.h>
#include <opencog/atoms/value/FloatValue.h>

/** \addtogroup grp_atomspace
 *  @{
 */

class TruthValueUTest;

namespace opencog
{

// Truth-value components
typedef double strength_t;
typedef double confidence_t;
typedef double count_t;

/// Class to control the TV merging strategy
struct MergeCtrl
{
	/// Styles for controlling merging two different truth value types
	/// as described in https://github.com/opencog/opencog/issues/1295
	///
	/// Stronger means TV type with higher resolution. The weakest TV type
	/// would be simple TV, and the strongest would be distributional TV
	/// (to be (re)implemented).
	enum class TVType
	{
		OLDER,
		NEWER,
		STRONGER,
		WEAKER
	};

	/// Styles for controlling the formula while merging two different
	/// truth values.
	enum class TVFormula
	{
		HIGHER_CONFIDENCE,  // TV with higher confidence overwrite the other
		PLN_BOOK_REVISION   // PLN book Section 5.10.2 revision rule
	};

	TVFormula tv_formula;
	TVType tv_type;

	MergeCtrl(TVFormula tvf=TVFormula::PLN_BOOK_REVISION,
	          TVType tvt=TVType::OLDER)
		: tv_formula(tvf), tv_type(tvt) {}
};

class TruthValue;
typedef std::shared_ptr<const TruthValue> TruthValuePtr;

class TruthValue
	: public FloatValue
{
	friend class Atom;

	// the TruthValueUTest class needs to access private members from the
	// TruthValue class, so we declare it as a friend class.
	friend class ::TruthValueUTest;

	// Disallow assignment -- truth values are immutable!
	TruthValue& operator=(const TruthValue& rhs) {
		throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
	}

protected:
	TruthValue(Type t) : FloatValue(t) {}

	// Merge helper method
	TruthValuePtr higher_confidence_merge(const TruthValuePtr&) const;

	static bool nearly_equal(double, double);

public:
	virtual ~TruthValue() {}

	virtual bool operator==(const Value&) const = 0;

	static TruthValuePtr factory(Type, const std::vector<double>&);
	static TruthValuePtr factory(const ValuePtr&);

	virtual std::string to_short_string(const std::string&) const;

	// Special TVs

	/**
	 * The shared reference to a special TRUE (Simple) TruthValue
	 * object with MAX_TRUTH mean and MAX_TV_CONFIDENCE count. That is,
	 * its true with absolute confidence.
	 */
	static TruthValuePtr TRUE_TV();
	/**
	 * The shared reference to a special default (Simple) TruthValue
	 * object with MAX_TRUTH mean and 0 count.  That is, its true,
	 * but with no confidence.
	 */
	static TruthValuePtr DEFAULT_TV();
	/**
	 * The shared reference to a special FALSE (Simple) TruthValue
	 * object with 0 mean and MAX_TV_CONFIDENCE count. That is, its
	 * false with absolute confidence.
	 */
	static TruthValuePtr FALSE_TV();
	/**
	 * The shared reference to a special TRIVIAL (Simple) TruthValue
	 * object with 0 mean and 0 count. That is, its false, but with
	 * no confidence.
	 */
	static TruthValuePtr TRIVIAL_TV();

	virtual strength_t get_mean()  const = 0;
	virtual confidence_t get_confidence()  const = 0;
	virtual count_t get_count()  const = 0;

	/**
	 * Merge this TV object with the given TV object argument.
	 * It always returns a new TV object with the result of the merge,
	 * even if it is equal to one of the merged TV objects.
	 * @param ms the merge style as described in
	 *        https://github.com/opencog/opencog/issues/1295
	 */
	virtual TruthValuePtr merge(const TruthValuePtr&,
	                            const MergeCtrl& = MergeCtrl()) const = 0;

	/**
	 * Check if this TV is equal to the default TV.
	 * operator!= only compares pointers.
	 */
	virtual bool isDefaultTV() const;
	virtual bool isDefinedTV() const;
};

static inline TruthValuePtr TruthValueCast(const ValuePtr& pa)
    { return std::dynamic_pointer_cast<const TruthValue>(pa); }

static inline ValuePtr ValueCast(const TruthValuePtr& tv)
{
	// This should have worked!?
	// return std::const_pointer_cast<Value>(tv);

	// This, too, should have worked!?
	// return std::shared_ptr<Value>(tv, const_cast<Value*>(tv.get()));

	// This works...
	return std::shared_ptr<Value>(tv, (Value*) tv.get());
}

typedef std::vector<TruthValuePtr> TruthValueSeq;

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(TruthValuePtr tv,
                         const std::string& indent=empty_string);
std::string oc_to_string(const TruthValueSeq& tvs,
                         const std::string& indent=empty_string);

} // namespace opencog

/** @}*/
#endif // _OPENCOG_TRUTH_VALUE_H
