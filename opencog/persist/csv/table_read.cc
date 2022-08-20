/** table_read.cc --
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
#include <atomic>
#include <iomanip>

#include <boost/algorithm/string.hpp>
#include <boost/tokenizer.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/irange.hpp>

#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/iostreamContainer.h>
#include <opencog/util/oc_omp.h>
#include <opencog/util/comprehension.h>

#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/VoidValue.h>

#include "table_read.h"

using namespace opencog;

// -------------------------------------------------------

/**
 * Remove the carriage return (for DOS format).
 */
static void removeCarriageReturn(std::string& str)
{
	size_t s = str.size();
	if ((s > 0) && (str[s-1] == '\r'))
		str.resize(s-1);
}

/**
 * Remove non ASCII char at the begining of the string.
 */
static void removeNonASCII(std::string& str)
{
	while (str.size() && (unsigned char)str[0] > 127)
		str = str.substr(1);
}

// -------------------------------------------------------
// Return true if the character is one of the standard comment
// delimiters.  Here, we define a 'standard delimiter' as one
// of hash, bang or semicolon.
static bool is_comment(const char c)
{
	if ('#' == c) return true;
	if (';' == c) return true;
	if ('!' == c) return true;
	if ('\n' == c) return true;
	if ('\r' == c) return true;
	if (0 == c) return true;
	return false;
}

/// Get one line of actual data.
/// This ignores lines that start with a 'standard comment char'
///
//
// TODO: This routine should be extended so that comments that start
// somewhere other than column 0 are also ignored.
//
// The signature of this routine is the same as std:getline()
//
std::istream& get_data_line(std::istream& is, std::string& line)
{
	while (true)
	{
		getline(is, line);
		if (!is) return is;
		if (is_comment(line[0])) continue;

		// Remove weird symbols at the start of the line (only).
		removeNonASCII(line);
		// Remove carriage return at end of line (for DOS files).
		removeCarriageReturn(line);

		return is;
	}
}

// -------------------------------------------------------

/**
 * Take a row, return a tokenizer.  Tokenization uses the
 * separator characters comma, blank, tab (',', ' ' or '\t').
 */
table_tokenizer opencog::get_row_tokenizer(const std::string& line)
{
	typedef boost::escaped_list_separator<char> separator;
	typedef boost::tokenizer<separator> tokenizer;

	// Tokenize line; currently, we allow tabs, commas, blanks.
	static const separator sep("\\", ",\t ", "\"");
	return tokenizer(line, sep);
}

// Same as above, but only allow commas as a column separator.
table_tokenizer get_sparse_row_tokenizer(const std::string& line)
{
	typedef boost::escaped_list_separator<char> separator;
	typedef boost::tokenizer<separator> tokenizer;

	// Tokenize line; currently, we allow tabs, commas, blanks.
	static const separator sep("\\", ",", "\"");
	return tokenizer(line, sep);
}

/**
 * Take a line and return a vector containing the elements parsed.
 * Used by istreamTable. This will modify the line to remove leading
 * non-ASCII characters, as well as stripping of any carriage-returns.
 */
std::vector<std::string> tokenizeSparseRow(const std::string& line)
{
	table_tokenizer tok = get_sparse_row_tokenizer(line);
	std::vector<std::string> res;
	for (std::string t : tok) {
		boost::trim(t);
		res.push_back(t);
	}
	return res;
}

// -------------------------------------------------------
/**
 * Given an input string, guess the type of the string.
 * Inferable types are: boolean, contin and enum.
 */
Type infer_type_from_token(const std::string& token)
{
    /* Prefered representation is T's and 0's, to maximize clarity,
     * readability.  Numeric values are easily confused with floating
     * point type.
     */
    if (token == "0" ||
        token == "1" ||
        token == "T" ||
        token == "F" ||
        token == "t" ||
        token == "f")
        return BOOL_VALUE;

    // If it starts with an alphabetic character, assume its a string
    else if (isalpha(token[0]))
        return STRING_VALUE;

    // Hope that we can cast this to a float point number.
    else {
        try {
            boost::lexical_cast<double>(token);
            return FLOAT_VALUE;
        }
        catch(...) {
            return VOID_VALUE;
        }
    }
}

/**
 * Given an input string, guess the type of the string.
 * Inferable types are: boolean, contin and enum.
 * Compare this to 'curr_guess', and upgrade the type inference
 * if it can be done consistently.
 */
static Type
infer_type_from_token2(Type curr_guess, const std::string& token)
{
    Type tokt = infer_type_from_token(token);

    // First time, just go with the flow.
    if (VOID_VALUE == curr_guess)
        return tokt;

    // Yayy! its consistent!
    if (tokt == curr_guess)
        return tokt;

    // If we saw 0,1 when expecting a contin, its a contin.
    if ((FLOAT_VALUE == curr_guess) && (BOOL_VALUE == tokt))
        return curr_guess;

    // If we thought its a boolean 0,1 it might be a contin.
    if ((BOOL_VALUE == curr_guess) && (FLOAT_VALUE == tokt))
        return tokt;

    // If we got to here, then there's some sort of unexpected
    // inconsistency in the column types; we've got to presume that
    // its just some crazy ascii string, i.e. enum_type.
    return STRING_VALUE;
}

// ===========================================================
#ifdef NOT_USED_ANYWHERE

// istream regular tables.
static const char *sparse_delim = " : ";

/**
 * Fill the input table, given a file in DSV (delimiter-seperated values)
 * format.  The delimiters are ',', ' ' or '\t'.
 *
 * It stuffs all data into the table as strings; type conversion to
 * the appropriate type, and thunking for the header, and ignoring
 * certain features, must all be done as a separate step.
 */
std::istream& istreamRawITable(std::istream& in, ITable& tab,
                               const std::vector<unsigned>& ignored_indices)
{
	std::streampos beg = in.tellg();

	// Get the entire dataset into memory
	std::string line;
	std::vector<std::string> lines;

	// Read first few by hand. The first might be labels, so we must
	// get at least the second line. But the second line might have
	// all default feature values (i.e. no colon), so get the third...
	dorepeat(20)
	{
		if (!get_data_line(in, line))
			break;

		// If it is a sparse file, we are outta here.
		// Throw an std::exception, since we don't want to log this as an
		// error (all the other exception types log to the log file).
		if (std::string::npos != line.find (sparse_delim))
		{
			in.seekg(beg);
			throw std::exception();
		}
		lines.push_back(line);
	}

	// Grab the rest of the file.
	while (get_data_line(in, line))
		lines.push_back(line);

	// Determine the arity from the first line.
	std::vector<std::string> fl = tokenizeRow(lines[0]);
	size_t arity = fl.size();

	std::atomic<int> arity_fail_row(-1);
	auto parse_line = [&](size_t i)
	{
		// tokenize the line and fill the table with
		tab[i] = tokenizeRow(lines[i]);

		// Check arity
		if (arity != tab[i].size())
			arity_fail_row = i + 1;
	};

	// Vector of indices [0, lines.size())
	size_t ls = lines.size();
	tab.resize(ls);
	auto ir = boost::irange((size_t)0, ls);
	std::vector<size_t> indices(ir.begin(), ir.end());
	OMP_ALGO::for_each(indices.begin(), indices.end(), parse_line);

	if (-1 != arity_fail_row) {
		in.seekg(beg);
		throw SyntaxException(TRACE_INFO,
				  "ERROR: Input file inconsistent: the %uth row has "
				  "a different number of columns than the rest of the file.  "
				  "All rows should have the same number of columns.\n",
				  arity_fail_row.load());
	}
	return in;
}
#endif // NOT_USED_ANYWHERE

// ===========================================================

/**
 * Infer the column types of the input table. It is assumed the
 * table's rows are vector of strings.
 */
std::vector<Type> infer_column_types(const std::vector<string_seq>& tab)
{
	std::vector<string_seq>::const_iterator rowit = tab.begin();

	size_t arity = rowit->size();
	std::vector<Type> types(arity, VOID_VALUE);

	// Skip the first line, it might be a header...
	// and that would confuse type inference.
	if (tab.size() > 1)
		++rowit;

	// Loop over all rows; this performs a consistency check.
	for (; rowit != tab.end(); ++rowit)
	{
		const string_seq& tokens = *rowit;
		for (size_t i=0; i<arity; i++)
			types[i] = infer_type_from_token2(types[i], tokens[i]);
	}
	return types;
}

/**
 * Infer the column types of a line and compare it to the given column
 * types.  If there is a mis-match, then it must be a header, i.e. a
 * set of ascii column labels.
 */
static bool
is_header(const string_seq& tokens, const std::vector<Type>& col_types)
{
	for (size_t i = 0; i < tokens.size(); i++)
	{
		Type flt = infer_type_from_token2(col_types[i], tokens[i]);
		if ((STRING_VALUE == flt) && (STRING_VALUE != col_types[i]))
			return true;
	}
	return false;
}

// ==================================================================

/**
 * Get indices (aka positions or offsets) of a list of labels given a
 * header. The labels can be sequenced in any order, it will always
 * return the order consistent with the header.
 */
static std::vector<unsigned>
get_indices(const string_seq &labels,
            const string_seq &header)
{
   std::vector<unsigned> res;
   for (size_t i = 0; i < header.size(); ++i)
      if (std::find(labels.begin(), labels.end(), header[i]) != labels.end())
         res.push_back(i);
   return res;
}

// ==================================================================

static std::istream&
inferTableAttributes(std::istream& in,
                     const std::vector<std::string>& ignore_features,
                     std::vector<unsigned>& ignore_idxs,
                     std::vector<Type>& tt,
                     std::vector<std::string>& maybe_header,
                     bool& has_header)
{
	has_header = false;

	std::streampos beg = in.tellg();

	// maxline is the maximum number of lines to read to infer the
	// attributes. A negative number means reading all lines.
	int maxline = 20;

	// Get a portion of the dataset into memory (cleaning weird stuff)
	std::vector<std::string> lines;
	std::string line;
	while (get_data_line(in, line) and 0 < maxline--)
		lines.push_back(line);

	// Parse what could be a header
	maybe_header = tokenizeRow(lines.front());

	// Determine arity
	size_t arity = maybe_header.size();
	std::atomic<int> arity_fail_row(-1);

	// Determine initial type
	std::vector<Type> types(arity, VOID_VALUE);

	// Parse the rest, determine its type and whether the arity is
	// consistent
	for (size_t i = 1; i < lines.size(); ++i)
	{
		// Parse line
		const string_seq& tokens = tokenizeRow(lines[i]);

		// Check arity
		if (arity != tokens.size())
		{
			arity_fail_row = i + 1;
			in.seekg(beg);
			in.clear();		 // in case it has reached the eof
			throw SyntaxException(TRACE_INFO,
				"ERROR: Input file inconsistent: the %uth row has a "
				"different number of columns than the rest of the file.  "
				"All rows should have the same number of columns.\n",
				arity_fail_row.load());
		}

		// Infer type
		boost::transform(types, tokens, types.begin(),
		                 infer_type_from_token2);
	}

	// Determine has_header
	has_header = is_header(maybe_header, types);

	// Determine type signature
	if (has_header)
	{
		ignore_idxs = get_indices(ignore_features, maybe_header);
		boost::sort(ignore_idxs);
	}

	in.seekg(beg);
	in.clear();		 // in case it has reached the eof
	return in;
}

// ==================================================================

/// cast string "token" to a vertex of type "tipe"
static bool token_to_bool(const std::string& token)
{
	if ("0" == token || "F" == token || "f" == token)
		return false;

	if ("1" == token || "T" == token || "t" == token)
		return true;

	throw SyntaxException(TRACE_INFO,
		"Expecting boolean value, got %s", token.c_str());
}

static double token_to_contin(const std::string& token)
{
	try {
		return boost::lexical_cast<double>(token);
	} catch (boost::bad_lexical_cast&) {
		throw SyntaxException(TRACE_INFO,
			"Could not cast %s to floating point", token.c_str());
	}
}


static std::istream&
istreamDenseTable(const Handle& anchor,
                  std::istream& in,
                  const std::vector<unsigned>& ignore_idxs,
                  const std::vector<Type>& col_types,
                  const std::vector<std::string>& header,
                  bool has_header)
{
	// Width of table in the input.
	size_t table_width = col_types.size();

	// Effective width is the width, without the ignored columns.
	// size_t effective_width = table_width - ignore_idxs.size();

	// Setup a mask; should we skip the column?
	std::vector<bool> skip_col(table_width, false);
	for (unsigned i : ignore_idxs)
		skip_col[i] = true;

	// Set up typed columns.
	std::vector<std::vector<bool>> bool_cols;
	std::vector<std::vector<double>> float_cols;
	std::vector<std::vector<std::string>> string_cols;

	for (size_t ic = 0; ic < table_width; ic++)
	{
		if (skip_col[ic]) continue;
		if (BOOL_VALUE == col_types[ic])
			bool_cols.push_back(std::vector<bool>());
		else
		if (FLOAT_VALUE == col_types[ic])
			float_cols.push_back(std::vector<double>());
		else
		if (STRING_VALUE == col_types[ic])
			string_cols.push_back(std::vector<std::string>());
		else
			throw RuntimeException(TRACE_INFO,
				"Unhandled column type");
	}

	std::string line;

	// Assume the stream is at the begining.
	// If there is a header, skip one line.
	if (has_header)
		get_data_line(in, line);

	// Loop over all lines in the table, one by one.
	while (get_data_line(in, line))
	{
		table_tokenizer toker = get_row_tokenizer(line);
		size_t ic = 0;
		size_t bc = 0;
		size_t fc = 0;
		size_t sc = 0;
		for (const std::string& tok : toker)
		{
			if (skip_col[ic]) { ic++; continue; }
			if (BOOL_VALUE == col_types[ic])
			{
				bool_cols[bc].push_back(token_to_bool(tok));
				bc ++;
				ic ++;
				continue;
			}

			if (FLOAT_VALUE == col_types[ic])
			{
				float_cols[fc].push_back(token_to_contin(tok));
				fc ++;
				ic ++;
				continue;
			}

			if (STRING_VALUE == col_types[ic])
			{
				string_cols[sc].push_back(tok);
				sc ++;
				ic ++;
				continue;
			}

			throw RuntimeException(TRACE_INFO,
				"Unhandled column type");
		}
	}

	return in;
}

// ==================================================================

/**
 * Perform 2 passes:
 *
 * 1) Infer
 * 1.1) its type
 * 1.2) whether it has a header
 * 1.3) whether it is dense or sparse
 *
 * 2) Load the actual data.
 */
std::istream&
opencog::istreamTable(const Handle& anchor,
                      std::istream& in,
                      const std::vector<std::string>& ignore_features)
{
	std::streampos beg = in.tellg();

	// Infer the properties of the table without loading its content
	std::vector<unsigned> ignore_indexes;
	std::vector<Type> col_types;
	std::vector<std::string> header;
	bool has_header = false;
	inferTableAttributes(in, ignore_features, ignore_indexes,
	                     col_types, header, has_header);

	// If the header is missing, then fake it.
	if (not has_header)
	{
		header.clear();
		for (size_t i=0; i<col_types.size(); i++)
			header.push_back("c" + std::to_string(i));
	}

	in.seekg(beg);

	return istreamDenseTable(anchor, in, ignore_indexes,
		col_types, header, has_header);
}

// ==================================================================

void loadTable(const Handle& anchor,
               const std::string& file_name,
               const string_seq& ignore_features)
{
	if (file_name.empty())
		throw RuntimeException(TRACE_INFO, "The file name is empty!");
	std::ifstream in(file_name.c_str());
	if (not in.is_open())
		throw RuntimeException(TRACE_INFO,
			"Could not open %s", file_name.c_str());

    istreamTable(anchor, in, ignore_features);
}

// ==================================================================
