/*
 * opencog/persist/proxy/ProxyNode.h
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

#ifndef _OPENCOG_PROXY_NODE_H
#define _OPENCOG_PROXY_NODE_H

#include <opencog/persist/api/StorageNode.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */

class ProxyNode : public StorageNode
{
private:
	void init(void);

public:
	ProxyNode(const std::string&&);
	ProxyNode(Type t, const std::string&&);
	virtual ~ProxyNode();

	StorageNodeSeq setup();

	// Flags. Avoid calling into the proxy, if these are absent.
	bool have_getAtom;
	bool have_fetchIncomingSet;
	bool have_fetchIncomingByType;
	bool have_storeAtom;
	bool have_removeAtom;
	bool have_storeValue;
	bool have_updateValue;
	bool have_loadValue;
	bool have_loadType;
	bool have_loadAtomSpace;
	bool have_storeAtomSpace;

	// ----------------------------------------------------------------
	// Assorted gorp. Provide som stop-gap methods, for now.
	// Probably needs fixing, later. XXX FIXME.
	virtual void create(void) {}

	virtual void destroy(void);
	virtual void erase(void);

	virtual std::string monitor(void);

protected:
	// ----------------------------------------------------------------
	// XXX FIXME Unimplemented BackingStore virtuals.
	// These need to go into the assorted implementations,
	// But its all very confusing and tedious, so punt.

	virtual HandleSeq loadFrameDAG(void);
	virtual void storeFrameDAG(AtomSpace*) {}

	virtual void deleteFrame(AtomSpace*) {}
	virtual Handle getLink(Type, const HandleSeq&);
};

NODE_PTR_DECL(ProxyNode)
#define createProxyNode CREATE_DECL(ProxyNode)

/** @}*/
} // namespace opencog

extern "C" {
void opencog_persist_proxy_init(void);
};

#endif // _OPENCOG_PROXY_NODE_H
