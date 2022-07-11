/*
 * fast_load.h
 * fast load of Atomese in s-expression format.
 *
 * Copyright (C) 2020 Alexey Potapov, Anatoly Belikov
 *
 * Authors: Alexey Potapov
 *          Anatoly Belikov
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

#ifndef FAST_LOAD_H
#define FAST_LOAD_H

#include <istream>
#include <string>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{
    void load_file(const std::string& file_name, AtomSpacePtr);
    static inline void load_file(const std::string& file_name, AtomSpace& asr)
        { load_file(file_name, AtomSpaceCast(&asr)); }

    Handle parseExpression(const std::string& expr, AtomSpacePtr);
    static inline Handle parseExpression(const std::string& expr, AtomSpace& asr)
        { return parseExpression(expr, AtomSpaceCast(&asr)); }

    Handle parseStream(std::istream&, AtomSpacePtr);
    static inline Handle parseStream(std::istream& stm, AtomSpace& asr)
        { return parseStream(stm, AtomSpaceCast(&asr)); }
}

#endif // FAST_LOAD_H
