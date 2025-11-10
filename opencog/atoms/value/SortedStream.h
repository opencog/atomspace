/*
 * opencog/atoms/value/SortedStream.h
 *
 * Copyright (C) 2025 BrainyBlaze Dynamics, Inc.
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

#ifndef _OPENCOG_SORTED_STREAM_H
#define _OPENCOG_SORTED_STREAM_H

#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/value/UnisetValue.h>
#include <opencog/atoms/flow/ValueShimLink.h>
#include <thread>

namespace opencog
{

/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * A LinkValue that maintains contents in sorted order.
 * Sort order is determined by the sort schema.
 * This must be executable, take exactly two arguments,
 * and must return a crisp bool value, indicating the order.
 */
class SortedStream
	: public UnisetValue
{
protected:
	// Ordering schema and its evaluation
	Handle _schema;
	ValueShimLinkPtr _left_shim;
	ValueShimLinkPtr _right_shim;
	Handle _exout;
	AtomSpace* _scratch;

	// Data source, and the thread that pulls from it.
	LinkValuePtr _source;
	std::thread _puller;
	void drain(void);
	void drainloop(void);

	void init_cmp(void);
	void init_src(const ValuePtr&);

	virtual void update() const override;
	virtual bool less(const Value& lhs, const Value& rhs) const override;

public:
	SortedStream(const Handle&);
	SortedStream(const HandleSeq&);
	SortedStream(const ValueSeq&);
	virtual ~SortedStream();

	virtual void add(const ValuePtr&) override;
	virtual void add(ValuePtr&&) override;
	virtual std::string to_string(const std::string& indent = "") const;
};

VALUE_PTR_DECL(SortedStream);
CREATE_VALUE_DECL(SortedStream);

/** @}*/
} // namespace opencog

#endif // _OPENCOG_SORTED_STREAM_H
