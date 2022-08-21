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

#include <opencog/atomspace/AtomSpace.h>

namespace opencog {

// TODO: Should this be a StringValue?
typedef std::vector<std::string> string_seq;

/**
 * Load columns from a CSV file and place them into Atomese Values on
 * the indicated Atom. Atomese Values are vectors (of floats, bools,
 * strings, or more complex structures). Each Value holds one column
 * from the dataset.
 *
 * The features (columns) specified in ignore_features will be omitted
 * from the representation.
 *
 * For example, a CSV dataset like this:
 *    o, i1, i2, i3, i4
 *    1, 0, 0, 3.3, "foo"
 *    0, 1, 0, 4.4, "bar"
 *
 * will be loaded as key-value pairs on the `anchor` Atom.
 *
 * First, at the "well known location"
 *    (Predicate "*-column-keys-*")
 * there will be a list of all of the column-keys in the table:
 *    (LinkValue
 *       (Predicate "o")
 *       (Predicate "i1")
 *       (Predicate "i2")
 *       (Predicate "i3")
 *       (Predicate "i4"))
 *
 * Next, under each key, there will a column of values:
 *    (Predicate "o") (BoolValue 1 0)
 *    (Predicate "i1") (BoolValue 0 1)
 *    (Predicate "i2") (BoolValue 0 0)
 *    (Predicate "i3") (FloatValue 3.3 4.4)
 *    (Predicate "i4") (StringValue "foo" "bar")
 *
 * @param file_name
 * @param ignore_features
 */
void load_csv_table(const AtomSpacePtr&,
                    const Handle& anchor,
                    const std::string& file_name,
                    const string_seq& ignore_features=string_seq());

//std::istream& istreamRawITable(
//    std::istream& in, ITable& table,
//    const std::vector<unsigned>& ignored_indices=std::vector<unsigned>());

// Same as above, but works for an already-open stream.
std::istream& istreamTable(const AtomSpacePtr&,
                           const Handle&,
                           std::istream&,
                           const string_seq& ignore_features);

} // ~namespaces opencog

#endif // _ATOMESE_TABLE_READ_H
