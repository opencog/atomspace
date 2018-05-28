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

GDTV::GDTV(GDTVrep rep)
: ProtoAtom(CONDITIONAL_GENERALIZED_DISTRIBUTIONAL_TRUTH_VALUE)
{
    gdtv = rep;
}

GDTV ConditionalGDTV::getUnconditonal(double val)
{
    for (auto gdtvpart : gdtv)
    {
        Interval interval = std::get<0>(gdtvpart);

        if (std::get<0>(interval)<= val &&  val <= std::get<1>(interval))
        {
            return std::get<1>(gdtvpart);
        }

    }
}

/*
GDTV ConditionalGDTV::getUnconditonal(Interval);

GDTV ConditionalGDTV::getUnconditonal(GDTV condDist)
{
    IntervalCounts res = UniformGDTV(sdt:get<1>(gdtv[0]));
    for (auto gdtvpart : gdtv)
    {
        Interval interval = std::get<0>(gdtvpart);
        double intervalMiddle = (std::get<0>(interval) + std::get<1>(interval))/2;
        double weight = condDist.getMode(intervalMiddle);
        res.AddEvidence(std::get<1>gdtvpart * weigth);
    }
}
*/
