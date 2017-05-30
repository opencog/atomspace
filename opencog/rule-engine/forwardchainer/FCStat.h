/*
 * FCStat.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
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

#ifndef _FCSTAT_H_
#define _FCSTAT_H_

#include <opencog/atoms/base/Handle.h>
#include <map>

#include "../Rule.h"

namespace opencog {

struct InferenceRecord
{
	const Handle hsource;
	const Rule& rule;
	UnorderedHandleSet product;

	InferenceRecord(Handle h, const Rule& r, const UnorderedHandleSet& p)
		: hsource(h), rule(r), product(p) {}
};

class FCStat
{
private:
	std::vector<InferenceRecord> _inf_rec;
	AtomSpace& _as;

public:
	FCStat(AtomSpace& as) : _as(as) {}

	/**
	 * Record the inference step into memory, as well as in the
	 * atomspace according to the following format:
	 *
	 * ExecutionLink
	 *    <rule>
	 *    <step>
	 *    <source>
	 *    <product>
	 *
	 * where
	 *
	 * 1. <rule> is DefinedSchemaNode <rule-name>
	 * 2. <rule> is NumberNode <#iteration>
	 * 3. <source> is the source
	 * 4. <product> is a SetLink <p1> ... <pn> where pi are the products
	 */
	void add_inference_record(unsigned iteration, Handle source,
	                          const Rule& rule,
	                          const UnorderedHandleSet& product);
	UnorderedHandleSet get_all_products();
};

}

#endif /* _FCSTAT_H_ */
