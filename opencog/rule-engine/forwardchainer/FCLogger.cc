/*
 * FCLogger.cc
 *
 * Copyright (C) 2016 OpenCog Foundation
 *
 * Author: Nil Geisweiller
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

#include "FCLogger.h"

using namespace opencog;

// Create and return the single instance
Logger& opencog::fc_logger()
{
	auto fc_logger_instantiate = []() {
		Logger tmp(logger());
		tmp.set_component("ForwardChainer");
		// tmp.set_level(Logger::FINE);
		return tmp;
	};
	static Logger fc_instance(fc_logger_instantiate());
    return fc_instance;
}
