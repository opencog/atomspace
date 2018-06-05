/*
 * opencog/truthvalue/ConditionalGDTV.cc
 *
 * Copyright (C) 2018 SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#include <float.h>
#include <math.h>
#include <stdio.h>

#include <opencog/truthvalue/ConditionalGDTV.h>

using namespace opencog;

ConditionalGDTV::ConditionalGDTV()
: ProtoAtom(CONDITIONAL_GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
}

ConditionalGDTV::ConditionalGDTV(GDTVrep rep)
: ProtoAtom(CONDITIONAL_GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    value = rep;
}

GDTVPtr ConditionalGDTV::getUnconditional(Handle h)
{
    return std::make_shared<const GDTV>(value.find(h)->second);
}

GDTVPtr ConditionalGDTV::getUnconditional(GDTVPtr condDist)
{
    HandleCounter res;
    for (auto gdtvpart : value)
    {
        res += gdtvpart.second * condDist->value.get(gdtvpart.first,0);
    }
    return std::make_shared<const GDTV>(res);
}
