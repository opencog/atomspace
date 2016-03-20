/*
 * BCLogger.h
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

#ifndef BCLOGGER_H_
#define BCLOGGER_H_

#include <opencog/util/Logger.h>

namespace opencog
{

// singleton instance (following Meyer's design pattern)
Logger& bc_logger();

// Macros to not evaluate the stream if log level is disabled
#define LAZY_BC_LOG_ERROR if(bc_logger().is_error_enabled()) bc_logger().error()
#define LAZY_BC_LOG_WARN if(bc_logger().is_warn_enabled()) bc_logger().warn()
#define LAZY_BC_LOG_INFO if(bc_logger().is_info_enabled()) bc_logger().info()
#define LAZY_BC_LOG_DEBUG if(bc_logger().is_debug_enabled()) bc_logger().debug()
#define LAZY_BC_LOG_FINE if(bc_logger().is_fine_enabled()) bc_logger().fine()

} // ~namespace opencog

#endif /* BCLOGGER_H_ */
