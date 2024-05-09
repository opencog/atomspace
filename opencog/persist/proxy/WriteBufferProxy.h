/*
 * opencog/persist/proxy/WriteBufferProxy.h
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

#ifndef _OPENCOG_WRITE_BUFFER_PROXY_H
#define _OPENCOG_WRITE_BUFFER_PROXY_H

#include <thread>
#include <opencog/util/concurrent_set.h>
#include <opencog/persist/proxy/WriteThruProxy.h>

namespace opencog
{
/** \addtogroup grp_atomspace
 *  @{
 */
class WriteBufferProxy : public WriteThruProxy
{
private:
	// Performance-monitoring stats
	void reset_stats(void);
	size_t _nstalls;
	size_t _nbars;
	size_t _ndumps;
	size_t _astore;
	size_t _vstore;
	double _mavg_in_atoms;
	double _mavg_in_values;
	double _mavg_qu_atoms;
	double _mavg_qu_values;
	double _mavg_out_atoms;
	double _mavg_out_values;
	double _mavg_load;

protected:
	double _decay;
	size_t _high_water_mark;
	concurrent_set<Handle> _atom_queue;
	concurrent_set<std::pair<Handle,Handle>> _value_queue;
	std::thread _write_thread;
	bool _stop;
	void write_loop();
	void erase_recursive(const Handle&);

private:
	void init(void);

public:
	WriteBufferProxy(const std::string&&);
	WriteBufferProxy(Type, const std::string&&);
	virtual ~WriteBufferProxy();

	// ----------------------------------------------------------------
	virtual void open(void);
	virtual void close(void);
	virtual bool connected(void) { return  0 < _targets.size(); }

protected:
	// ----------------------------------------------------------------
	// BackingStore virtuals.

	virtual void storeAtom(const Handle&, bool synchronous = false);
	virtual void preRemoveAtom(AtomSpace*, const Handle&, bool recursive);
	virtual void postRemoveAtom(AtomSpace*, const Handle&,
	                            bool recursive, bool exok);
	virtual void storeValue(const Handle& atom, const Handle& key);
	virtual void updateValue(const Handle& atom, const Handle& key,
	                         const ValuePtr& delta);

	virtual void barrier(AtomSpace* = nullptr);
	virtual std::string monitor(void);

public:
	static Handle factory(const Handle&);
};

NODE_PTR_DECL(WriteBufferProxy)
#define createWriteBufferProxy CREATE_DECL(WriteBufferProxy)

/** @}*/
} // namespace opencog

#endif // _OPENCOG_WRITE_BUFFER_PROXY_H
