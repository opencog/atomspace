/*
 * opencog/atoms/value/Value.h
 *
 * Copyright (C) 2015 Linas Vepstas
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

#ifndef _OPENCOG_VALUE_H
#define _OPENCOG_VALUE_H

#include <memory>
#include <string>

#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

class Value;
typedef std::shared_ptr<Value> ValuePtr;

/**
 * Values are the base class for the Atom shared pointer.
 */
class Value
	: public std::enable_shared_from_this<Value>
{
protected:
	// We store the type locally, to avoid the overhead of
	// turning getType into a virtual method.
	Type _type;

public:
	Value(Type t) : _type(t) {}

	virtual ~Value() {}

	inline Type get_type() const { return _type; }

	virtual bool is_atom() const { return false; }
	virtual bool is_node() const { return false; }
	virtual bool is_link() const { return false; }
	virtual bool is_unordered_link() const { return false; }
	virtual size_t size() const { return 0; }
	virtual ValuePtr value_at_index(size_t) const = 0;

	/** Basic predicate */
	bool is_type(Type t, bool subclass = true) const
	{
		Type at(get_type());
		if (not subclass) return t == at;
		return nameserver().isA(at, t);
	}

	/**
	 * Returns a string representation of the value.
	 */
	virtual std::string to_string(const std::string& indent) const = 0;
	virtual std::string to_short_string(const std::string& indent) const
		{ return to_string(indent); }

	// Work around gdb's inability to build a string on the fly,
	// see http://stackoverflow.com/questions/16734783 for more
	// explanation.
	std::string to_string() const { return to_string(""); }
	std::string to_short_string() const { return to_short_string(""); }

	/**
	 * Returns whether two values are equal.
	 *
	 * @return true if the values are equal, false otherwise.
	 */
	virtual bool operator==(const Value&) const = 0;

	/**
	 * Returns whether two values are different.
	 *
	 * @return true if the values are different, false otherwise.
	 */
	bool operator!=(const Value& other) const
		{ return not operator==(other); }
};

// update() might throw an exception, e.g. an IOExcpetion if a
// StorageNode is not open. The problem is that if this was called
// from the guile shell, there is no handler that can catch this,
// and so guile will core-dump. So we install a handler now, and
// avoid this ugly fate.
#define SAFE_UPDATE(PRTSTR,PRINTER) \
try { \
	update(); \
	{ PRINTER; } \
} catch (const StandardException& ex) { \
	PRTSTR += " \""; \
	PRTSTR += ex.what(); \
	PRTSTR += "\""; \
}

typedef std::vector<ValuePtr> ValueSeq;
typedef std::set<ValuePtr> ValueSet;
typedef std::map<Handle, ValuePtr> ValueMap;

// Debugging helpers see
// http://wiki.opencog.org/w/Development_standards#Print_OpenCog_Objects
// The reason indent is not an optional argument with default is
// because gdb doesn't support that, see
// http://stackoverflow.com/questions/16734783 for more explanation.
std::string oc_to_string(const ValuePtr& vp,
                         const std::string& indent=empty_string);
std::string oc_to_string(const ValueSeq& vs,
                         const std::string& indent=empty_string);
std::string oc_to_string(const ValueMap& vs,
                         const std::string& indent=empty_string);

/**
 * Cast ValuePtr to the specific Value subclass. This function is defined only
 * for T which are subclasses of Value.
 */
template<typename T>
static inline
typename std::enable_if< std::is_base_of<Value, T>::value, std::shared_ptr<T> >::type
CastFromValue(const ValuePtr& value)
{
	return std::dynamic_pointer_cast<T>(value);
}

/**
 * Cast specific Value subclass to ValuePtr. This function is defined only
 * for T which are subclasses of Value.
 */
template<typename T>
static inline
typename std::enable_if< std::is_base_of<Value, T>::value, ValuePtr >::type
CastToValue(const std::shared_ptr<const T>& value)
{
	return std::dynamic_pointer_cast<Value>(std::const_pointer_cast<T>(value));
}

class Atom;

/**
 * createValue() function. Creates Values of a specific type, using
 * the appropriate constructor. This function is defined only for
 * C++ types T which are subclasses of Value.
 */
template<typename T, typename ... Args>
static inline
typename std::enable_if<
	std::is_base_of<Value, T>::value && !std::is_base_of<Atom, T>::value,
	std::shared_ptr<T> >::type
createValue(Args&&... args) {
	return std::make_shared<T>(std::forward<Args>(args)...);
}

/** @}*/
} // namespace opencog

// overload of operator<< to print Values
namespace std
{
    template<typename Out>
    Out& operator<<(Out& out, const opencog::ValuePtr& pa)
    {
        out << pa->to_string("");
        return out;
    }
} // ~namespace std

#endif // _OPENCOG_VALUE_H
