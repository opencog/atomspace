/*
 * LoggerSCM.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#include "LoggerSCM.h"

#include <opencog/util/Logger.h>

using namespace opencog;

LoggerSCM::LoggerSCM() : ModuleWrap("opencog logger") {}

/// This is called while (opencog logger) is the current module.
/// Thus, all the definitions below happen in that module.
void LoggerSCM::init(void)
{
	new FunctionWrap(do_logger_set_level, "cog-logger-set-level", "logger");
	new FunctionWrap(do_logger_get_level, "cog-logger-get-level", "logger");
	new FunctionWrap(do_logger_set_filename, "cog-logger-set-filename", "logger");
	new FunctionWrap(do_logger_get_filename, "cog-logger-get-filename", "logger");
	new FunctionWrap(do_logger_set_stdout, "cog-logger-set-stdout", "logger");
	new FunctionWrap(do_logger_error, "cog-logger-error", "logger");
	new FunctionWrap(do_logger_warn, "cog-logger-warn", "logger");
	new FunctionWrap(do_logger_info, "cog-logger-info", "logger");
	new FunctionWrap(do_logger_debug, "cog-logger-debug", "logger");
	new FunctionWrap(do_logger_fine, "cog-logger-fine", "logger");
}

namespace opencog {

void do_logger_set_level(const std::string& level)
{
	logger().setLevel(Logger::getLevelFromString(level));
}

const std::string& do_logger_get_level()
{
	static std::string level_str;
	level_str = Logger::getLevelString(logger().getLevel());
	return level_str;;
}

void do_logger_set_filename(const std::string& filename)
{
	logger().setFilename(filename);
}

const std::string& do_logger_get_filename()
{
	return logger().getFilename();
}

void do_logger_set_stdout(bool enable)
{
	return logger().setPrintToStdoutFlag(enable);
}

void do_logger_error(const std::string& msg)
{
	logger().error(msg);
}

void do_logger_warn(const std::string& msg)
{
	logger().warn(msg);
}

void do_logger_info(const std::string& msg)
{
	logger().info(msg);
}

void do_logger_debug(const std::string& msg)
{
	logger().debug(msg);
}

void do_logger_fine(const std::string& msg)
{
	logger().fine(msg);
}

} /*end of namespace opencog*/

void opencog_logger_init(void)
{
    static LoggerSCM logger_scm;
    logger_scm.module_init();
}
