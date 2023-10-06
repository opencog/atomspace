/*
 * opencog/persist/proxy/WriteThruProxy.h
 *
 * Copyright (C) 2022 Linas Vepstas
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

#ifndef _OPENCOG_WRITE_THRU_PROXY_H
#define _OPENCOG_WRITE_THRU_PROXY_H

#include <opencog/persist/proxy/ProxyNode.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class WriteThruProxy : public ProxyNode
{
private:
	StorageNodeSeq _targets;
	void init(void);

public:
	WriteThruProxy(const std::string&&);
	WriteThruProxy(Type, const std::string&&);
	virtual ~WriteThruProxy();

	// ----------------------------------------------------------------
	virtual void open(void);
	virtual void close(void);
	virtual bool connected(void) { return  0 < _targets.size(); }

protected:
	// ----------------------------------------------------------------
	// BackingStore virtuals.

	virtual void getAtom(const Handle&) {}
	virtual void fetchIncomingSet(AtomSpace*, const Handle&) {}
	virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type) {}
	virtual void storeAtom(const Handle&, bool synchronous = false);
	virtual void removeAtom(AtomSpace*, const Handle&, bool recursive);
	virtual void preRemoveAtom(AtomSpace*, const Handle&, bool recursive);
	virtual void postRemoveAtom(AtomSpace*, const Handle&,
	                            bool recursive, bool exok);
	virtual void storeValue(const Handle& atom, const Handle& key);
	virtual void updateValue(const Handle& atom, const Handle& key,
	                         const ValuePtr& delta);
	virtual void loadValue(const Handle& atom, const Handle& key) {}

	virtual void loadType(AtomSpace*, Type) {}
	virtual void loadAtomSpace(AtomSpace*) {}
	virtual void storeAtomSpace(const AtomSpace*) {}
	virtual void barrier(AtomSpace* = nullptr);

public:
	static Handle factory(const Handle&);
};

NODE_PTR_DECL(WriteThruProxy)
#define createWriteThruProxy CREATE_DECL(WriteThruProxy)

/** @}*/
} // namespace opencog

#endif // _OPENCOG_WRITE_THRU_PROXY_H
