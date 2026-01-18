/*
 * opencog/atoms/core/ObjectNode.h
 *
 * Copyright (C) 2025 Linas Vepstas
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
 */

#ifndef _OPENCOG_OBJECT_NODE_H
#define _OPENCOG_OBJECT_NODE_H

#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>

#include <string>
#include <unordered_map>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// Provide some basic management for Object messages.
/// This will sort-of-ish, kind-of reduce some cut-n-paste now
/// prolferating across various projects that derive from this class.
/// Seems like an OK base class to have, as I write this, but the c++
/// "Curiously Recurring Template Pattern" (CRTP) is new to me, and
/// I don't know if I like it. Experimental. We shall see what happens.
class ObjectNode : public Node
{
protected:
	/// Implement Jenkins' One-at-a-Time hash.
	/// For these very short strings, I cannot think of a faster hash.
	/// The 4-byte-at-a-time hashes require knowng the string length :-(
	static uint32_t constexpr dispatch_hash(const char* s)
	{
		uint32_t hash = 0;

		for(; *s; ++s)
		{
			hash += *s;
			hash += (hash << 10);
			hash ^= (hash >> 6);
		}

		hash += (hash << 3);
		hash ^= (hash >> 11);
		hash += (hash << 15);

		return hash;
	}

	ObjectNode(Type, const std::string&&);

	/**
	 * Return debug diagnostics and/or performance monitoring stats.
	 */
   virtual std::string monitor(void) const;

public:
	virtual HandleSeq getMessages() const = 0;
	virtual bool usesMessage(const Handle&) const = 0;
};

/// Curiously Recurring Template Pattern -- CRTP
///
template <typename Derived>
class ObjectCRTP : public ObjectNode
{
protected:
	static std::unordered_map<std::string, Handle> _msgs;
	static bool _init;
	static void do_init(void)
	{
		if (_init) return;
		_init = true;
		for (const char* msg : Derived::_messages)
			addMessage(msg);
	}

	ObjectCRTP(Type t, const std::string&& name) :
		ObjectNode(t, std::move(name))
	{
		do_init();
	}

	static void addMessage(const char* str)
	{
		Handle h(createNode(PREDICATE_NODE, str));
		h->markIsMessage();
		_msgs.emplace(str, std::move(h));
	}

public:

	virtual HandleSeq getMessages() const override
	{
		// Copy list above into the local AtomSpace.
		HandleSeq lms;
		for (const auto& kv : _msgs)
			lms.emplace_back(_atom_space->add_atom(kv.second));
		return lms;
	}

	virtual bool usesMessage(const Handle& key) const override
	{
		if (PREDICATE_NODE != key->get_type()) return false;

		const std::string& pred = key->get_name();
		return _msgs.find(pred) != _msgs.end();
	}
};

template <typename Derived>
std::unordered_map<std::string, Handle> ObjectCRTP<Derived>::_msgs;

template <typename Derived>
bool ObjectCRTP<Derived>::_init = false;

/** @}*/
} // namespace opencog

#endif // _OPENCOG_OBJECT_NODE_H
