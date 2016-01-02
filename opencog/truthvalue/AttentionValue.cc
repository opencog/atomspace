/*
 * opencog/truthvalue/AttentionValue.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * All Rights Reserved
 *
 * Written by Tony Lofthouse <tony_lofthouse@btinternet.com>
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

#include <limits>

#include "AttentionValue.h"

using namespace opencog;

const AttentionValue::sti_t AttentionValue::DEFAULTATOMSTI = 0;
const AttentionValue::lti_t AttentionValue::DEFAULTATOMLTI = 0;
const AttentionValue::vlti_t AttentionValue::DEFAULTATOMVLTI = 0;

void AttentionValue::decaySTI()
{
    // Prevent m_STI from wrapping around.
    if (m_STI > std::numeric_limits<sti_t>::min()) m_STI--;
}

std::string AttentionValue::toString() const
{
    char buffer[256];
    sprintf(buffer, "[%d, %d, %s]", (int)m_STI, (int)m_LTI,
            m_VLTI ? "NONDISPOSABLE" : "DISPOSABLE");
    return buffer;
}
