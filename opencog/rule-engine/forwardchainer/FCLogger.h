/*
 * FCLogger.h
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

#ifndef FCLOGGER_H_
#define FCLOGGER_H_

#include <opencog/util/Logger.h>

namespace opencog
{

// singleton instance (following Meyer's design pattern)
Logger& fc_logger();

// Macros to not evaluate the stream if log level is disabled
#define LAZY_FC_LOG_ERROR if(fc_logger().isErrorEnabled()) fc_logger().error()
#define LAZY_FC_LOG_WARN if(fc_logger().isWarnEnabled()) fc_logger().warn()
#define LAZY_FC_LOG_INFO if(fc_logger().isInfoEnabled()) fc_logger().info()
#define LAZY_FC_LOG_DEBUG if(fc_logger().isDebugEnabled()) fc_logger().debug()
#define LAZY_FC_LOG_FINE if(fc_logger().isFineEnabled()) fc_logger().fine()

} // ~namespace opencog

#endif /* FCLOGGER_H_ */
