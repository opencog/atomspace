/**
 * table_read.h -- Read a CSV/TSV table
 *
 * Copyright (C) 2010 OpenCog Foundation
 * Copyright (C) 2012 Poulin Holdings LLC
 * Copyright (C) 2022 Linas Vepstas
 *
 * Authors: Nil Geisweiller <ngeiswei@gmail.com>
 *          Linas Vepstas <linasvepstas@gmail.com>
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

#ifndef _ATOMESE_TABLE_READ_H
#define _ATOMESE_TABLE_READ_H

#include <fstream>
#include <string>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/tokenizer.hpp>

#include <opencog/atoms/value/Value.h>

namespace opencog {

// TODO: Should this be a StringValue?
typedef std::vector<std::string> string_seq;

// ===========================================================

//std::istream& istreamRawITable(
//    std::istream& in, ITable& table,
//    const std::vector<unsigned>& ignored_indices=std::vector<unsigned>());

std::istream& istreamTable(const Handle&,
                           std::istream&,
                           const string_seq& ignore_features);

void loadTable(const Handle& anchor,
               const std::string& file_name,
               const string_seq& ignore_features=string_seq());

} // ~namespaces opencog

#endif // _ATOMESE_TABLE_READ_H
