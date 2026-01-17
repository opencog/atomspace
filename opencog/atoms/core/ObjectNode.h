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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_OBJECT_NODE_H
#define _OPENCOG_OBJECT_NODE_H

#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>

#include <string>
#include <unordered_set>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

// Dispatcher for setValue, getValue.
class ObjectNode : public Node
{
protected:
	static uint32_t constexpr dispatch_hash(const char*);

	ObjectNode(Type, const std::string&&);
	virtual void addMessage(const char*) const = 0;

	/**
	 * Return debug diagnostics and/or performance monitoring stats.
	 */
   virtual std::string monitor(void) const;

public:
	virtual ~ObjectNode() = default;

	virtual HandleSeq getMessages() const = 0;
	virtual bool usesMessage(const Handle&) const = 0;
};

/// Curiously Recurring Template Pattern -- CRTP
///
template <typename Derived>
class ObjectCRTP : public ObjectNode
{
protected:
	static std::unordered_set<uint32_t> msgset;
	static HandleSeq preds;

	virtual void addMessage(const char* str) const override
	{
		msgset.insert(dispatch_hash(str));
		Handle h(createNode(PREDICATE_NODE, str));
		h->markIsMessage();
		preds.emplace_back(std::move(h));
	}

public:

	virtual HandleSeq getMessages() const override
	{
		// Copy list above into the local AtomSpace.
		HandleSeq lms;
		for (const Handle& m : preds)
			lms.emplace_back(_atom_space->add_atom(m));
		return lms;
	}

	virtual bool usesMessage(const Handle& key) const override
	{
		if (PREDICATE_NODE != key->get_type()) return false;

		const std::string& pred = key->get_name();
		uint32_t dhsh = dispatch_hash(pred.c_str());
		if (msgset.find(dhsh) != msgset.end()) return true;
		return false;
	}
};


/** @}*/
} // namespace opencog

#endif // _OPENCOG_OBJECT_NODE_H
