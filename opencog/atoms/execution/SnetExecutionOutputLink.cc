/*
 * opencog/atoms/execution/SnetExecutionOutputLink.cc
 *
 * Copyright (C) 2019 Vitaly Bogdanov <vsbogd@gmail.com>
 * All Rights Reserved
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

#include "SnetExecutionOutputLink.h"

using namespace opencog;

SnetExecutionOutputLink::SnetExecutionOutputLink(const HandleSeq& oset, Type t)
	: ExecutionOutputLink(oset, t)
{
}

Handle SnetExecutionOutputLink::execute(AtomSpace* as, bool silent) const
{
	return ExecutionOutputLink::execute(as, silent);
}

DEFINE_LINK_FACTORY(SnetExecutionOutputLink, SNET_EXECUTION_OUTPUT_LINK)

