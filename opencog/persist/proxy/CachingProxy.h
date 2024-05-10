/*
 * opencog/persist/proxy/CachingProxy.h
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

#ifndef _OPENCOG_CACHING_PROXY_H
#define _OPENCOG_CACHING_PROXY_H

#include <opencog/persist/proxy/ReadThruProxy.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class CachingProxy : public ReadThruProxy
{
private:
	void init(void);

public:
	CachingProxy(const std::string&&);
	CachingProxy(Type t, const std::string&&);
	virtual ~CachingProxy();

	// ----------------------------------------------------------------
	virtual void open(void);
	virtual void close(void);

protected:
	// ----------------------------------------------------------------
	// BackingStore virtuals.

	virtual void getAtom(const Handle&);
	virtual void fetchIncomingSet(AtomSpace*, const Handle&);
	virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type);
	virtual void loadValue(const Handle& atom, const Handle& key);
	virtual void loadType(AtomSpace*, Type);

public:
	static Handle factory(const Handle&);
};

NODE_PTR_DECL(CachingProxy)
#define createCachingProxy CREATE_DECL(CachingProxy)

/** @}*/
} // namespace opencog

#endif // _OPENCOG_CACHING_PROXY_H
