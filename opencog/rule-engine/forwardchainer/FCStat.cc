/*
 * FCStat.cc
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

#include "../ChainerUtils.h"
#include "FCStat.h"

using namespace opencog;

void FCStat::add_inference_record(unsigned iteration, Handle source,
                                  const Rule& rule,
                                  const UnorderedHandleSet& product)
{
	_inf_rec.emplace_back(source, rule, product);

	for (const Handle& p : product)
		_as.add_link(EXECUTION_LINK,
		             rule.get_alias(),
		             _as.add_node(NUMBER_NODE, std::to_string(iteration)),
		             source, p);
}

UnorderedHandleSet FCStat::get_all_products()
{
	UnorderedHandleSet all;
	for(const auto& ir : _inf_rec)
		all.insert(ir.product.begin(),ir.product.end());

	return all;
}
