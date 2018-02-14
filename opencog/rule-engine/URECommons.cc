/*
 * URECommons.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Oct 2014
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

#include <opencog/atoms/base/Link.h>

#include "URECommons.h"

using namespace opencog;

URECommons::URECommons(AtomSpace& as) : _as(as) {}

double URECommons::tv_fitness(const Handle& h) const
{
	TruthValuePtr ptv(h->getTruthValue());
	confidence_t c = ptv->get_confidence();
	strength_t s = ptv->get_mean();
	return (pow(s, FITNESS_PARAM) * (pow(c, (2 - FITNESS_PARAM))));
}
