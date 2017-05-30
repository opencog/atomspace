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

#include <opencog/util/Logger.h>
#include <opencog/guile/SchemeModule.h>
#include "SchemePrimitive.h"

using namespace opencog;
namespace opencog {

/**
 * Expose the Logger singleton to Scheme
 */

class LoggerSCM : public ModuleWrap
{
protected:
	virtual void init();

	std::string do_logger_set_level(const std::string& level);
	std::string do_logger_get_level(void);
	std::string do_logger_set_filename(const std::string& filename);
	std::string do_logger_get_filename(void);
	bool do_logger_set_stdout(bool);
	bool do_logger_set_sync(bool);
	bool do_logger_set_timestamp(bool);
	void do_logger_error(const std::string& msg);
	void do_logger_warn(const std::string& msg);
	void do_logger_info(const std::string& msg);
	void do_logger_debug(const std::string& msg);
	void do_logger_fine(const std::string& msg);

public:
	LoggerSCM();
};


/// Set level, return previous level.
std::string LoggerSCM::do_logger_set_level(const std::string& level)
{
	std::string prev_level;
	prev_level = Logger::get_level_string(logger().get_level());
	logger().set_level(Logger::get_level_from_string(level));
	return prev_level;
}

std::string LoggerSCM::do_logger_get_level(void)
{
	return Logger::get_level_string(logger().get_level());
}

/// Set logfile, return previous file.
std::string LoggerSCM::do_logger_set_filename(const std::string& filename)
{
	std::string old_file;
	old_file = logger().get_filename();
	logger().set_filename(filename);
	return old_file;
}

std::string LoggerSCM::do_logger_get_filename()
{
	return logger().get_filename();
}

bool LoggerSCM::do_logger_set_stdout(bool enable)
{
	// bool previous_setting = logger().get_print_to_stdout_flag();
	bool previous_setting = enable;
	logger().set_print_to_stdout_flag(enable);
	return previous_setting;
}

bool LoggerSCM::do_logger_set_sync(bool enable)
{
	// bool previous_setting = logger().get_sync_flag();
	bool previous_setting = enable;
	logger().set_sync_flag(enable);
	return previous_setting;
}

bool LoggerSCM::do_logger_set_timestamp(bool enable)
{
	// bool previous_setting = logger().get_timestamp_flag();
	bool previous_setting = enable;
	logger().set_timestamp_flag(enable);
	return previous_setting;
}

void LoggerSCM::do_logger_error(const std::string& msg)
{
	logger().error(msg);
}

void LoggerSCM::do_logger_warn(const std::string& msg)
{
	logger().warn(msg);
}

void LoggerSCM::do_logger_info(const std::string& msg)
{
	logger().info(msg);
}

void LoggerSCM::do_logger_debug(const std::string& msg)
{
	logger().debug(msg);
}

void LoggerSCM::do_logger_fine(const std::string& msg)
{
	logger().fine(msg);
}

} /*end of namespace opencog*/

LoggerSCM::LoggerSCM() : ModuleWrap("opencog logger") {}

/// This is called while (opencog logger) is the current module.
/// Thus, all the definitions below happen in that module.
void LoggerSCM::init(void)
{
	define_scheme_primitive("cog-logger-set-level!",
		&LoggerSCM::do_logger_set_level, this, "logger");
	define_scheme_primitive("cog-logger-get-level",
		&LoggerSCM::do_logger_get_level, this, "logger");
	define_scheme_primitive("cog-logger-set-filename!",
		&LoggerSCM::do_logger_set_filename, this, "logger");
	define_scheme_primitive("cog-logger-get-filename",
		&LoggerSCM::do_logger_get_filename, this, "logger");
	define_scheme_primitive("cog-logger-set-stdout!",
		&LoggerSCM::do_logger_set_stdout, this, "logger");
	define_scheme_primitive("cog-logger-set-sync!",
		&LoggerSCM::do_logger_set_sync, this, "logger");
	define_scheme_primitive("cog-logger-set-timestamp!",
		&LoggerSCM::do_logger_set_timestamp, this, "logger");
	define_scheme_primitive("cog-logger-error-str",
		&LoggerSCM::do_logger_error, this, "logger");
	define_scheme_primitive("cog-logger-warn-str",
		&LoggerSCM::do_logger_warn, this, "logger");
	define_scheme_primitive("cog-logger-info-str",
		&LoggerSCM::do_logger_info, this, "logger");
	define_scheme_primitive("cog-logger-debug-str",
		&LoggerSCM::do_logger_debug, this, "logger");
	define_scheme_primitive("cog-logger-fine-str",
		&LoggerSCM::do_logger_fine, this, "logger");
}

extern "C" {
void opencog_logger_init(void);
};

void opencog_logger_init(void)
{
    static LoggerSCM logger_scm;
    logger_scm.module_init();
}
