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
#include <boost/range/algorithm/find.hpp>
#include <boost/range/algorithm/count_if.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/irange.hpp>
#include <boost/tokenizer.hpp>
#include <boost/variant.hpp>

#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>

#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/iostreamContainer.h>
#include <opencog/util/oc_omp.h>
#include <opencog/util/comprehension.h>

#include <opencog/atoms/value/BoolValue.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/value/VoidValue.h>

#include "table_read.h"

using namespace opencog;

using namespace boost;
using namespace boost::phoenix;
using boost::phoenix::arg_names::arg1;

// -------------------------------------------------------

/**
 * Return true if the next chars in 'in' correspond to carriage return
 * (support UNIX and DOS format) and advance in of the checked chars.
 */
static bool checkCarriageReturn(std::istream& in)
{
	char next_c = in.get();
	if (next_c == '\r') // DOS format
		next_c = in.get();
	if (next_c == '\n')
		return true;
	return false;
}

/**
 * remove the carriage return (for DOS format)
 */
static void removeCarriageReturn(std::string& str)
{
	size_t s = str.size();
	if ((s > 0) && (str[s-1] == '\r'))
		str.resize(s-1);
}

/**
 * remove non ASCII char at the begining of the string
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

static const char *sparse_delim = " : ";

/**
 * parse a pair of key/value in a parse dataset, using ':' as
 * delimiter. For instance
 *
 * parse_key_val("key : val")
 *
 * returns
 *
 * {"key", "val"}
 *
 * If no such delimiter is found then it return a pair with empty key
 * and empty val.
 */
static std::pair<std::string, std::string>
parse_key_val(const std::string& chunk)
{
	std::pair<std::string, std::string> res;
	size_t pos = chunk.find(sparse_delim);
	if (std::string::npos == pos)
		return res;
	std::string key = chunk.substr(0, pos);
	boost::trim(key);
	std::string val = chunk.substr(pos + strlen(sparse_delim));
	boost::trim(val);
	return {key, val};
}

/**
 * Take a row, return a tokenizer.  Tokenization uses the
 * separator characters comma, blank, tab (',', ' ' or '\t').
 */
table_tokenizer get_row_tokenizer(const std::string& line)
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

/// cast string "token" to a vertex of type "tipe"
static ValuePtr token_to_boolean(const std::string& token)
{
	if ("0" == token || "F" == token || "f" == token)
		return createBoolValue(false);

	if ("1" == token || "T" == token || "t" == token)
		return createBoolValue(true);

	throw SyntaxException(TRACE_INFO,
		"Expecting boolean value, got %s", token.c_str());
}

static ValuePtr token_to_contin(const std::string& token)
{
	try {
		return createFloatValue(lexical_cast<double>(token));
	} catch (boost::bad_lexical_cast&) {
		throw SyntaxException(TRACE_INFO,
			"Could not cast %s to floating point", token.c_str());
	}
}

ValuePtr opencog::token_to_vertex(Type tipe, const std::string& token)
{
	if (BOOL_VALUE == tipe)
		return token_to_boolean(token);

	if (FLOAT_VALUE == tipe)
		return token_to_contin(token);

	if (STRING_VALUE == tipe)
	{
		// Enum types must begin with an alpha character
		if (isalpha(token[0]))
			return createStringValue(token);

		throw SyntaxException(TRACE_INFO,
			"Enum type must begin with alphabetic char, but %s doesn't",
			token.c_str());
	}

	throw SyntaxException(TRACE_INFO,
		"Unable to convert token \"%s\" to type=%d",
		token.c_str(), tipe);
}

// ===========================================================
// istream regular tables.

/**
 * Fill the input table, given a file in DSV (delimiter-seperated values)
 * format.  The delimiters are ',', ' ' or '\t'.
 *
 * It stuffs all data into the table as strings; type conversion to
 * the appropriate type, and thunking for the header, and ignoring
 * certain features, must all be done as a separate step.
 */
std::istream& istreamRawITable(std::istream& in, std::vector<string_seq>& tab,
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
	std::vector<std::string> fl = tokenizeRow<std::string>(lines[0], ignored_indices);
	size_t arity = fl.size();

	std::atomic<int> arity_fail_row(-1);
	auto parse_line = [&](size_t i)
	{
		// tokenize the line and fill the table with
		tab[i] = tokenizeRow<std::string>(lines[i], ignored_indices);

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

std::vector<std::string> get_header(const std::string& file_name)
{
	std::ifstream in(file_name.c_str());
	std::string line;
	get_data_line(in, line);
	return tokenizeRow<std::string>(line);
}

// ===========================================================
/**
 * Visitor to parse a list of strings (buried in a multi_type_seq)
 * into a multi_type_seq containing the typed values given the input
 * type signature.
 */
struct from_tokens_visitor : public boost::static_visitor<multi_type_seq>
{
    from_tokens_visitor(const type_node_seq& types) : _types(types) {
        all_boolean = boost::count(types, id::boolean_type) == (int)types.size();
        all_contin = boost::count(types, id::contin_type) == (int)types.size();
    }
    result_type operator()(const string_seq& seq) {
        result_type res;
        if (all_boolean) {
            res = builtin_seq();
            builtin_seq& bs = res.get_seq<builtin>();
            boost::transform(seq, back_inserter(bs), token_to_boolean);
        }
        else if (all_contin) {
            res = contin_seq();
            contin_seq& cs = res.get_seq<contin_t>();
            boost::transform(seq, back_inserter(cs), token_to_contin);
        }
        else {
            res = vertex_seq();
            vertex_seq& vs = res.get_seq<vertex>();
            boost::transform(_types, seq, back_inserter(vs), token_to_vertex);
        }
        return res;
    }
    template<typename Seq> result_type operator()(const Seq& seq) {
        OC_ASSERT(false, "You are not supposed to do that");
        return result_type();
    }
    const type_node_seq& _types;
    bool all_boolean, all_contin;
};


/**
 * The class below tokenizes one row, and jams it into the table
 */
struct from_sparse_tokens_visitor : public from_tokens_visitor
{
    from_sparse_tokens_visitor(const type_node_seq& types,
                               const std::map<const std::string, size_t>& index,
                               size_t fixed_arity)
        : from_tokens_visitor(types), _index(index), _fixed_arity(fixed_arity) {}
    result_type operator()(const string_seq& seq) {
        using std::transform;
        using std::for_each;
        result_type res;
        if (all_boolean) {
            res = builtin_seq(_types.size(), id::logical_false);
            builtin_seq& bs = res.get_seq<builtin>();
            auto begin_sparse = seq.begin() + _fixed_arity;
            transform(seq.begin(), begin_sparse, bs.begin(), token_to_boolean);
            for (auto it = begin_sparse; it != seq.end(); ++it) {
                auto key_val = parse_key_val(*it);
                if (key_val != std::pair<std::string, std::string>()) {
                    size_t idx = _index.at(key_val.first);
                    bs[idx] = token_to_boolean(key_val.second);
                }
            }
        }
        else if (all_contin) {
            res = contin_seq(_types.size(), 0.0);
            contin_seq& cs = res.get_seq<contin_t>();
            auto begin_sparse = seq.cbegin() + _fixed_arity;
            transform(seq.begin(), begin_sparse, cs.begin(), token_to_contin);
            for (auto it = begin_sparse; it != seq.end(); ++it) {
                auto key_val = parse_key_val(*it);
                if (key_val != std::pair<std::string, std::string>()) {
                    size_t idx = _index.at(key_val.first);
                    cs[idx] = token_to_contin(key_val.second);
                }
            }
        }
        else {
            res = vertex_seq(_types.size());
            vertex_seq& vs = res.get_seq<vertex>();
            auto begin_sparse_types = _types.cbegin() + _fixed_arity;
            auto begin_sparse_seq = seq.cbegin() + _fixed_arity;
            transform(_types.begin(), begin_sparse_types,
                      seq.begin(), vs.begin(), token_to_vertex);
            for (auto it = begin_sparse_seq; it != seq.end(); ++it) {
                auto key_val = parse_key_val(*it);
                if (key_val != std::pair<std::string, std::string>()) {
                    size_t idx = _index.at(key_val.first);
                    vs[idx] = token_to_vertex(_types[idx], key_val.second);
                }
            }
        }
        return res;
    }
    std::map<const std::string, size_t> _index;
    size_t _fixed_arity;
};


// ===========================================================
/**
 * Fill the input table, given a file in 'sparse' format.
 *
 * The sparse table format consists of some fixed number of columns,
 * in comma-separated format, followed by key-value pairs, also
 * tab-separated. viz:
 *
 *     val, val, val, name:val, name:val, name:val
 *
 * Thus, for example, a row such as
 *
 *    earn, issued : 1, results : 2, ending : 1, including : 1
 *
 * indicates that there one fixed column, of enum type, (the enum value
 * being "earn"), and that features called "issued", "ending" and
 * "including" have a contin value of 1.0  and "results" has a contin
 * value of 2.
 *
 * The routine does NOT store the table in sparse format: it stores the
 * full, exploded table. This could be bad ...
 * TODO: we really need a sparse table format, as well.
 *
 * The "Raw" format has all data as strings; type conversion to the
 * appropriate type, must all be done as a separate step.
 */
istream& istreamSparseITable(istream& in, ITable& tab)
{
    // The raw dataset
    std::vector<string> lines;

    // The first non-comment line is assumed to be the header.
    // ... unless it isn't. (The header must not contain a colon).
    vector<string> labs;
    size_t fixed_arity = 0;
    std::string header;
    get_data_line(in, header);
    if (string::npos == header.find(sparse_delim)) {
        // Determine the arity of the fixed columns
        vector<string> hdr = tokenizeSparseRow(header);
        fixed_arity = hdr.size();
        labs = hdr;
    }
    else {
        lines.push_back(header);
    }

    // Get the entire dataset into memory
    std::string iline;
    while (get_data_line(in, iline))
        lines.push_back(iline);

    if (0 == fixed_arity) {
        vector<string> fixy = tokenizeSparseRow(lines[0]);
        // count commas, until a semi-colon is found.
        while (string::npos == fixy[fixed_arity].find(sparse_delim))
            fixed_arity++;
    }
    logger().info() << "Sparse file fixed column count=" << fixed_arity;

    // Get a list of all of the features.
    set<string> feats;
    // All sparse features have the same type.
    type_node feat_type = id::unknown_type;

    // Fixed features may have different types, by column.
    type_node_seq types(fixed_arity, id::unknown_type);

    for (const std::string& line : lines) {
        vector<string> chunks = tokenizeSparseRow(line);
        vector<string>::const_iterator pit = chunks.begin();

        // Infer the types of the fixed features.
        size_t off = 0;
        for (; off < fixed_arity; ++off, ++pit)
            types[off] = infer_type_from_token2(types[off], *pit);

        for (; pit != chunks.end(); ++pit) {
            // Rip out the key-value pairs
            auto key_val = parse_key_val(*pit);
            if (key_val == pair<string, std::string>())
                break;
            // Store the key, uniquely.  Store best guess as the type.
            feats.insert(key_val.first);
            feat_type = infer_type_from_token2(feat_type, key_val.second);
        }
    }
    logger().info() << "Sparse file unique features count=" << feats.size();
    logger().info() << "Sparse file feature type=" << feat_type;
    logger().info() << "Sparse file row count=" << lines.size();

    // Convert the feature set into a list of labels.
    // 'index' is a map from feature name to column number.
    size_t cnt = fixed_arity;
    std::map<const std::string, size_t> index;
    for (const std::string& key : feats) {
        types.push_back(feat_type);
        labs.push_back(key);
        index[key] = cnt;
        cnt++;
    }
    tab.set_labels(labs);
    tab.set_types(types);

    // And finally, stuff up the table.
    from_sparse_tokens_visitor fstv(types, index, fixed_arity);
    auto fill_line = [&](int i)
    {
        const std::string& line = lines[i];
        // Tokenize the line
        vector<string> chunks = tokenizeSparseRow(line);
        multi_type_seq row = fstv(chunks);
        tab[i] = row;
    };

    // Vector of indices [0, lines.size())
    size_t ls = lines.size();
    tab.resize(ls);
    auto ir = boost::irange((size_t)0, ls);
    vector<size_t> indices(ir.begin(), ir.end());
    OMP_ALGO::for_each(indices.begin(), indices.end(), fill_line);

    return in;
}

/**
 * Infer the column types of the input table. It is assumed the
 * table's rows are vector of strings.
 */
type_node_seq infer_column_types(const ITable& tab)
{
    vector<multi_type_seq>::const_iterator rowit = tab.begin();

    arity_t arity = rowit->size();
    type_node_seq types(arity, id::unknown_type);

    // Skip the first line, it might be a header...
    // and that would confuse type inference.
    if (tab.size() > 1)
        ++rowit;
    for (; rowit != tab.end(); ++rowit)
    {
        const string_seq& tokens = rowit->get_seq<string>();
        for (arity_t i=0; i<arity; i++)
            types[i] = infer_type_from_token2(types[i], tokens[i]);
    }
    return types;
}

/**
 * Infer the column types of the first line of a raw input table and
 * compare it to the given column types.  If there is a mis-match,
 * then the first row must be a header, i.e. a set of ascii column
 * labels.
 */
bool has_header(ITable& tab, type_node_seq col_types)
{
    const string_seq& row = tab.begin()->get_seq<string>();

    arity_t arity = row.size();

    for (arity_t i=0; i<arity; i++) {
        type_node flt = infer_type_from_token2(col_types[i], row[i]);
        if ((id::enum_type == flt) && (id::enum_type != col_types[i]))
            return true;
    }
    return false;
}

/**
 * Infer the column types of a line and compare it to the given column
 * types.  If there is a mis-match, then it must be a header, i.e. a
 * set of ascii column labels.
 */
bool is_header(const vector<string>& tokens, const type_node_seq& col_types)
{
    for (size_t i = 0; i < tokens.size(); i++) {
        type_node flt = infer_type_from_token2(col_types[i], tokens[i]);
        if ((id::enum_type == flt) && (id::enum_type != col_types[i]))
            return true;
    }
    return false;
}

/**
 * Fill the input table only, given a DSV (delimiter-seperated values)
 * file format, where delimiters are ',', ' ' or '\t'.
 *
 * This algorithm makes several passes over the data.  First, it reads
 * the entire table, as a collection of strings.  Next, it tries to
 * infer the column types, and the presence of a header.
 */
istream& istreamITable(istream& in, ITable& tab,
                       const vector<string>& ignore_features)
{
    try {
        istreamRawITable(in, tab);
    }
    catch (std::exception& e) {
        istreamSparseITable(in, tab);
        // Get rid of the unwanted columns.
        tab.delete_columns(ignore_features);
        return in;
    }

    // Determine the column types.
    type_node_seq col_types = infer_column_types(tab);
    tab.set_types(col_types);

    // If there is a header row, then it must be the column labels.
    if (has_header(tab, col_types)) {
        tab.set_labels(tab.begin()->get_seq<string>());
        tab.erase(tab.begin());
    }

    // Now that we have some column labels to work off of,
    // Get rid of the unwanted columns.
    tab.delete_columns(ignore_features);

    // Finally, perform a column type conversion
    from_tokens_visitor ftv(tab.get_types());
    auto aft = apply_visitor(ftv);
    OMP_ALGO::transform(tab.begin(), tab.end(), tab.begin(),
                        [&](multi_type_seq& seq) {
                            return aft(seq.get_variant());
                        });

    return in;
}

/**
 * Like istreamITable but add the option to ignore indices.
 *
 * It's akind of a temporary hack, till it's clear that this is much
 * faster and we should recode istreamITable to ignore features
 * head-on.
 *
 * Also, it assumes that the dataset is not sparse.
 */
istream& istreamITable_ignore_indices(istream& in, ITable& tab,
                                      const vector<unsigned>& ignore_indices)
{
    istreamRawITable(in, tab, ignore_indices);

    // Determine the column types.
    type_node_seq col_types = infer_column_types(tab);
    tab.set_types(col_types);

    // If there is a header row, then it must be the column labels.
    if (has_header(tab, col_types)) {
        tab.set_labels(tab.begin()->get_seq<string>());
        tab.erase(tab.begin());
    }

    // Finally, perform a column type conversion
    from_tokens_visitor ftv(tab.get_types());
    auto aft = apply_visitor(ftv);
    OMP_ALGO::transform(tab.begin(), tab.end(), tab.begin(),
                        [&](multi_type_seq& seq) {
                            return aft(seq.get_variant());
                        });

    return in;
}

/**
 * Take a line and return a triple with vector containing the input
 * elements, output element and timestamp.
 */
std::tuple<std::vector<std::string>, std::string, std::string>
tokenizeRowIOT(const std::string& line,
               const std::vector<unsigned>& ignored_indices,
               int target_idx,  // < 0 == ignored
               int timestamp_idx) // < 0 == ignored
{
    std::tuple<std::vector<std::string>, std::string, std::string> res;
    table_tokenizer toker = get_row_tokenizer(line);
    int i = 0;
    for (const std::string& tok : toker) {
        if (!boost::binary_search(ignored_indices, i)) {
            std::string el = boost::lexical_cast<string>(tok);
            if (target_idx == i)
                std::get<1>(res) = el;
            else if (timestamp_idx == i)
                std::get<2>(res) = el;
            else
                std::get<0>(res).push_back(el);
        }
        i++;
    }
    return res;
}

ITable loadITable(const std::string& file_name,
                  const vector<string>& ignore_features)
{
    OC_ASSERT(!file_name.empty(), "the file name is empty");
    ifstream in(file_name.c_str());
    OC_ASSERT(in.is_open(), "Could not open %s", file_name.c_str());

    ITable res;
    istreamITable(in, res, ignore_features);
    return res;
}

/**
 * Like loadITable but it is optimized by ignoring features head-on
 * (rather than loading them, then removing them.
 *
 * WARNING: it assumes the dataset has a header!!!
 */
ITable loadITable_optimized(const std::string& file_name,
                            const vector<string>& ignore_features)
{
    OC_ASSERT(!file_name.empty(), "the file name is empty");
    ifstream in(file_name.c_str());
    OC_ASSERT(in.is_open(), "Could not open %s", file_name.c_str());

    // determined ignore_indices
    vector<unsigned> ignore_indices = get_indices(ignore_features,
                                                  get_header(file_name));

    ITable res;
    istreamITable_ignore_indices(in, res, ignore_indices);
    return res;
}

/**
 * Fill an input table and output table given a DSV
 * (delimiter-seperated values) file format, where delimiters are ',',
 * ' ' or '\t'.
 *
 * It is assumed that each row have the same number of columns, if not
 * an assert is raised.
 *
 * pos specifies the position of the output, if -1 it is the last
 * position. The default position is 0, the first column.
 *
 * This is only used for sparse table and could be optimized
 */
istream& istreamTable_OLD(istream& in, Table& tab,
                          const std::string& target_feature,
                          const std::vector<std::string>& ignore_features)
{
    istreamITable(in, tab.itable, ignore_features);

    tab.otable = tab.itable.get_column_data(target_feature);
    OC_ASSERT(0 != tab.otable.size(),
              "Fatal Error: target feature \"%s\" not found",
              target_feature.c_str());

    tab.target_pos = tab.itable.get_column_offset(target_feature);

    type_node targ_type = tab.itable.get_type(target_feature);

    std::string targ_feat = tab.itable.delete_column(target_feature);

    tab.otable.set_label(targ_feat);
    tab.otable.set_type(targ_type);

    return in;
}

/**
 * Like istreamTable but optimize by ignoring features head-on rather
 * than loading them then removing them.
 *
 * Warning: only works on dense data with header file.
 */
istream& istreamTable_ignore_indices(istream& in, Table& tab,
                                     const std::string& target_feature,
                                     const std::vector<unsigned>& ignore_indices)
{
    istreamITable_ignore_indices(in, tab.itable, ignore_indices);

    tab.otable = tab.itable.get_column_data(target_feature);
    OC_ASSERT(0 != tab.otable.size(),
              "Fatal Error: target feature \"%s\" not found",
              target_feature.c_str());

    tab.target_pos = tab.itable.get_column_offset(target_feature);

    type_node targ_type = tab.itable.get_type(target_feature);

    std::string targ_feat = tab.itable.delete_column(target_feature);

    tab.otable.set_label(targ_feat);
    tab.otable.set_type(targ_type);

    return in;
}

// ==================================================================

static istream&
inferTableAttributes(istream& in, const std::string& target_feature,
                     const std::string& timestamp_feature,
                     const vector<string>& ignore_features,
                     type_tree& tt, bool& has_header, bool& is_sparse)
{
    // maxline is the maximum number of lines to read to infer the
    // attributes. A negative number means reading all lines.
    int maxline = 20;
    streampos beg = in.tellg();

    // Get a portion of the dataset into memory (cleaning weird stuff)
    std::vector<string> lines;
    {
        std::string line;
        is_sparse = false;
        while (get_data_line(in, line) && maxline-- > 0) {
            // It is sparse
            is_sparse = is_sparse || std::string::npos != line.find(sparse_delim);
            if (is_sparse) { // just get out
                // TODO could be simplified, optimized, etc
                in.seekg(beg);
                in.clear();         // in case it has reached the eof
                return in;
            }

            // put the line in a buffer
            lines.push_back(line);
        }
    }

    // parse what could be a header
    const vector<string> maybe_header = tokenizeRow<string>(lines.front());

    // determine arity
    arity_t arity = maybe_header.size();
    std::atomic<int> arity_fail_row(-1);

    // determine initial type
    type_node_seq types(arity, id::unknown_type);

    // parse the rest, determine its type and whether the arity is
    // consistent
    for (size_t i = 1; i < lines.size(); ++i) {
        // Parse line
        const string_seq& tokens = tokenizeRow<string>(lines[i]);

        // Check arity
        if (arity != (arity_t)tokens.size()) {
            arity_fail_row = i + 1;
            in.seekg(beg);
            in.clear();         // in case it has reached the eof
            OC_ASSERT(false,
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
    if (has_header) {

        // if unspecified, the target is the first column
        unsigned target_idx = 0;

        // target feature will be ignored
        if (!target_feature.empty()) {
            auto target_it = std::find(maybe_header.begin(), maybe_header.end(),
                                       target_feature);
            OC_ASSERT(target_it != maybe_header.end(), "Target %s not found",
                      target_feature.c_str());
            target_idx = std::distance(maybe_header.begin(), target_it);
        }
        vector<unsigned> ignore_idxs =
            get_indices(ignore_features, maybe_header);
        ignore_idxs.push_back(target_idx);
        boost::sort(ignore_idxs);

        // Include timestamp feature as idx to ignore
        if (!timestamp_feature.empty()) {
            auto timestamp_it = std::find(maybe_header.begin(), maybe_header.end(),
                                          timestamp_feature);
            OC_ASSERT(timestamp_it != maybe_header.end(),
                      "Timestamp feature  %s not found",
                      timestamp_feature.c_str());
            unsigned timestamp_idx = std::distance(maybe_header.begin(), timestamp_it);
            ignore_idxs.push_back(timestamp_idx);
            boost::sort(ignore_idxs);
        }

        // Generate type signature
        type_node otype = types[target_idx];
        type_node_seq itypes;
        for (unsigned i = 0; i < types.size(); ++i)
            if (!boost::binary_search(ignore_idxs, i))
                itypes.push_back(types[i]);
        tt = gen_signature(itypes, otype);
    } else {
        // No header, the target is the first column
        type_node otype = types[0];
        types.erase(types.begin());
        tt = gen_signature(types, otype);
    }
    logger().debug() << "Infered type tree: " << tt;

    in.seekg(beg);
    in.clear();         // in case it has reached the eof
    return in;
}

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
istream& istreamTable(istream& in, Table& tab,
                      const std::string& target_feature,
                      const std::string& timestamp_feature,
                      const std::vector<std::string>& ignore_features)
{
    // Infer the properties of the table without loading its content
    type_tree tt;
    bool has_header, is_sparse;
    streampos beg = in.tellg();
    inferTableAttributes(in, target_feature, timestamp_feature,
                         ignore_features, tt, has_header, is_sparse);
    in.seekg(beg);

    if (is_sparse) {
        // fallback on the old loader
        // TODO: this could definitely be optimized
        OC_ASSERT(timestamp_feature.empty(), "Timestamp feature not implemented");
        return istreamTable_OLD(in, tab, target_feature, ignore_features);
    } else {
        return istreamDenseTable(in, tab, target_feature, timestamp_feature,
                                 ignore_features, tt, has_header);
    }
}

// ==================================================================

/**
 * Take a line and return a pair with vector containing the input
 * elements and then output element.
 */
template<typename T>
std::pair<std::vector<T>, T>
tokenizeRowIO (
    const std::string& line,
    const std::vector<unsigned>& ignored_indices=std::vector<unsigned>(),
    unsigned target_idx=0)
{
    std::pair<std::vector<T>, T> res;
    table_tokenizer toker = get_row_tokenizer(line);
    size_t i = 0;
    for (const std::string& tok : toker) {
        if (!boost::binary_search(ignored_indices, i)) {
            T el = boost::lexical_cast<T>(tok);
            if (target_idx == i)
                res.second = el;
            else
                res.first.push_back(el);
        }
        i++;
    }
    return res;
}

// ==================================================================

static istream&
istreamDenseTable_noHeader(istream& in, Table& tab,
                           int target_idx, // < 0 == ignore
                           int timestamp_idx, // < 0 == ignore
                           const vector<unsigned>& ignore_idxs,
                           const type_tree& tt, bool has_header)
{
    // Get the entire dataset into memory (cleaning weird stuff)
    std::string line;
    std::vector<string> lines;
    while (get_data_line(in, line))
        lines.push_back(line);

    // Allocate all rows in the itable, otable and ttable
    tab.itable.resize(lines.size());
    tab.otable.resize(lines.size());
    if (timestamp_idx >= 0)
        tab.ttable.resize(lines.size());

    // Get the elementary io types
    type_node_seq itypes =
        vector_comp(get_signature_inputs(tt), get_type_node);
    type_node otype = get_type_node(get_signature_output(tt));

    // Assign the io type to the table
    tab.itable.set_types(itypes);
    tab.otable.set_type(otype);

    // Instantiate type conversion for inputs
    from_tokens_visitor ftv(itypes);

    // Function to parse each line (to be called in parallel)
    auto parse_line = [&](unsigned i) {
        try {
            // Fill input
            auto tokenIOT = tokenizeRowIOT(lines[i], ignore_idxs,
                                           target_idx, timestamp_idx);
            tab.itable[i] = ftv(std::get<0>(tokenIOT));

            // Fill output
            std::string output_str = std::get<1>(tokenIOT);
            // If there is no valid target index, then there is no
            // "output" column!
            if (""  != output_str)
                tab.otable[i] = token_to_vertex(otype, output_str);

            // Fill date
            std::string date_str = std::get<2>(tokenIOT);
            // If there is no valid timestamp index, then there is no
            // "output" column!
            if (""  != date_str)
                tab.ttable[i] = TTable::from_string(date_str);
        }
        catch (AssertionException& ex) {
            unsigned lineno = has_header? i+1 : i;
            OC_ASSERT(false, "Parsing error occurred on line %d of input file\n"
                             "Exception: %s", lineno, ex.what());
        }
    };

    // Call it for each line in parallel
    auto ir = boost::irange((size_t)0, lines.size());
    vector<size_t> row_idxs(ir.begin(), ir.end());
    OMP_ALGO::for_each(row_idxs.begin(), row_idxs.end(), parse_line);

    // Assign the target position relative to the ignored indices
    // (useful for writing that file back)
    tab.target_pos = target_idx - boost::count_if(ignore_idxs,
                                                  arg1 < target_idx);

    if (timestamp_idx >= 0)
        tab.timestamp_pos = timestamp_idx -
            boost::count_if(ignore_idxs, arg1 < timestamp_idx);

    return in;
}

istream& istreamDenseTable(istream& in, Table& tab,
                           const std::string& target_feature,
                           const std::string& timestamp_feature,
                           const vector<string>& ignore_features,
                           const type_tree& tt, bool has_header)
{
    OC_ASSERT(has_header
              || (target_feature.empty()
                  && ignore_features.empty()
                  && timestamp_feature.empty()),
              "If the data file has no header, "
              "then a target feature, ignore features or "
              "timestamp_feature cannot be specified");

    // determine target, timestamp and ignore indexes
    int target_idx = 0;    // if no header, target is at the first
                           // column by default

    int timestamp_idx = -1;     // disabled by default
    vector<unsigned> ignore_idxs;
    if (has_header) {
        std::string line;
        get_data_line(in, line);
        vector<string> header = tokenizeRow<string>(line);

        // Set target idx
        if (!target_feature.empty()) {
            auto target_it = std::find(header.begin(), header.end(),
                                       target_feature);
            OC_ASSERT(target_it != header.end(), "Target %s not found",
                      target_feature.c_str());
            target_idx = std::distance(header.begin(), target_it);
        }

        // Set timestamp idx
        if (!timestamp_feature.empty()) {
            auto timestamp_it = std::find(header.begin(), header.end(),
                                          timestamp_feature);
            OC_ASSERT(timestamp_it != header.end(), "Timestamp feature %s not found",
                      timestamp_feature.c_str());
            timestamp_idx = std::distance(header.begin(), timestamp_it);
        }

        // Set ignore idxs
        ignore_idxs = get_indices(ignore_features, header);

        // get input and output labels from the header
        auto iotlabels = tokenizeRowIOT(line, ignore_idxs,
                                        target_idx, timestamp_idx);
        tab.itable.set_labels(std::get<0>(iotlabels));
        tab.otable.set_label(std::get<1>(iotlabels));
        tab.ttable.set_label(std::get<2>(iotlabels));
    }

    return istreamDenseTable_noHeader(in, tab, target_idx, timestamp_idx,
                                      ignore_idxs, tt, has_header);
}

// ==================================================================

Table loadTable(const std::string& file_name,
                const std::string& target_feature,
                const std::string& timestamp_feature,
                const string_seq& ignore_features)
{
    OC_ASSERT(!file_name.empty(), "the file name is empty");
    ifstream in(file_name.c_str());
    OC_ASSERT(in.is_open(), "Could not open %s", file_name.c_str());

    Table res;
    istreamTable(in, res, target_feature, timestamp_feature, ignore_features);
    return res;
}

// ==================================================================
