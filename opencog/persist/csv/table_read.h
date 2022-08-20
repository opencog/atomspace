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
#include <boost/range/algorithm/count.hpp>
#include <boost/range/algorithm/binary_search.hpp>
#include <boost/range/algorithm_ext/for_each.hpp>
#include <boost/tokenizer.hpp>

#include <opencog/atoms/value/Value.h>

namespace opencog {

/**
 * remove the carriage return (for DOS format)
 */
void removeCarriageReturn(std::string& str);

/**
 * remove non ASCII char at the begining of the string
 */
void removeNonASCII(std::string& str);

/**
 * Return true if the next chars in 'in' correspond to carriage return
 * (support UNIX and DOS format) and advance in of the checked chars.
 */
bool checkCarriageReturn(std::istream& in);

/**
 * Convert strings to typed values
 */
ValuePtr token_to_boolean(const std::string&);
ValuePtr token_to_contin(const std::string&);
ValuePtr token_to_vertex(Type, const std::string&);


// ===========================================================

typedef boost::tokenizer<boost::escaped_list_separator<char>> table_tokenizer;

/**
 * Take a row, return a tokenizer.  Tokenization uses the
 * separator characters comma, blank, tab (',', ' ' or '\t').
 */
table_tokenizer get_row_tokenizer(const std::string& line);

/**
 * Take a line and return a vector containing the elements parsed.
 */
template<typename T>
static std::vector<T> tokenizeRow (
    const std::string& line,
    const std::vector<unsigned>& ignored_indices=std::vector<unsigned>())
{
    table_tokenizer tok = get_row_tokenizer(line);
    std::vector<T> res;
    unsigned i = 0;
    for (const std::string& t : tok) {

        // trim away whitespace padding; failing to do this
        // confuses stuff downstream.
        std::string clean(t);
        boost::trim(clean);

        // Sometimes the tokenizer returns pure whitespace :-(
        if (0 == clean.size()) continue;

        if (!boost::binary_search(ignored_indices, i++))
            res.push_back(boost::lexical_cast<T>(clean));
    }
    return res;
}

// ===========================================================

// TODO Should this be a TableValue?
class Table : public std::vector<ValuePtr>
{
	public:
		Table(void);
};

// ===========================================================
typedef std::vector<std::string> string_seq;

// Get the header of a DSV file (assuming there is one)
string_seq get_header(const std::string& input_file);

std::istream& istreamRawITable(
    std::istream& in, Table& tab,
    const std::vector<unsigned>& ignored_indices=std::vector<unsigned>());

std::istream& istreamITable(std::istream& in, Table& tab,
                           const string_seq& ignore_features);

std::istream& istreamTable(std::istream& in, Table& tab,
                           const string_seq& ignore_features);

// TODO: reimplement loadITable with the same model of loadTable and
// remove loadITable_optimized
Table loadITable(
    const std::string& file_name,
    const string_seq& ignore_features=string_seq());

Table loadITable_optimized(
    const std::string& file_name,
    const string_seq& ignore_features=string_seq());

/**
 * If target_feature is empty then, in case there is no header, it is
 * assumed to be the first feature.
 */
Table loadTable(
    const std::string& file_name,
    const string_seq& ignore_features=string_seq());

std::istream& istreamDenseTable(std::istream&, Table&,
                                const string_seq& ignore_features,
                                const std::vector<Type>&, bool has_header);


} // ~namespaces opencog

#endif // _ATOMESE_TABLE_READ_H
