/**
 * load_csv.h -- Load CSV tables into Values
 *
 * Copyright (C) 2018 OpenCog Foundation
 * Copyright (C) 2022 Linas Vepstas
 *
 * Author: Yidnekachew Wondimu <searchyidne@gmail.com>
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

#ifndef _ATOMESE_LOAD_CSV_H
#define _ATOMESE_LOAD_CSV_H

namespace opencog {

// Load columns from a CSV file and place them into Atomese Values on
// the indicated Atom. See the .cc file for additional info.
void load_csv_table(
	const Handle& anchor,
	const std::string& file_name,
	const std::vector<std::string>& ignore_features=std::vector<std::string>());

} // end namespace opencog

#endif //_ATOMESE_LOAD_CSV_H
