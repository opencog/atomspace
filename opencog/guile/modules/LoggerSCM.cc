/*
 * LoggerSCM.cc
 *
 * Copyright (C) 2015, 2024 OpenCog Foundation
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
#include "../SchemePrimitive.h"

using namespace opencog;
namespace opencog {

/**
 * Expose the Logger singleton to Scheme.
 * All functions operate on the global logger() singleton.
 */

class LoggerSCM : public ModuleWrap
{
protected:
	virtual void init();

	std::string do_logger_set_level(const std::string& level);
	std::string do_logger_get_level();
	std::string do_logger_set_filename(const std::string& filename);
	std::string do_logger_get_filename();
	std::string do_logger_set_component(const std::string& component);
	std::string do_logger_get_component();
	bool do_logger_set_stdout(bool);
	bool do_logger_set_sync(bool);
	bool do_logger_set_timestamp(bool);
	bool do_logger_is_error_enabled();
	bool do_logger_is_warn_enabled();
	bool do_logger_is_info_enabled();
	bool do_logger_is_debug_enabled();
	bool do_logger_is_fine_enabled();
	void do_logger_error(const std::string& msg);
	void do_logger_warn(const std::string& msg);
	void do_logger_info(const std::string& msg);
	void do_logger_debug(const std::string& msg);
	void do_logger_fine(const std::string& msg);
	void do_flush();

public:
	LoggerSCM();
};

/// Set level, return previous level.
std::string LoggerSCM::do_logger_set_level(const std::string& level)
{
	std::string prev = Logger::get_level_string(logger().get_level());
	logger().set_level(Logger::get_level_from_string(level));
	return prev;
}

std::string LoggerSCM::do_logger_get_level()
{
	return Logger::get_level_string(logger().get_level());
}

/// Set logfile, return previous file.
std::string LoggerSCM::do_logger_set_filename(const std::string& filename)
{
	std::string old = logger().get_filename();
	logger().set_filename(filename);
	return old;
}

std::string LoggerSCM::do_logger_get_filename()
{
	return logger().get_filename();
}

/// Set component, return previous component.
std::string LoggerSCM::do_logger_set_component(const std::string& component)
{
	std::string old = logger().get_component();
	logger().set_component(component);
	return old;
}

std::string LoggerSCM::do_logger_get_component()
{
	return logger().get_component();
}

bool LoggerSCM::do_logger_set_stdout(bool enable)
{
	bool prev = enable;
	logger().set_print_to_stdout_flag(enable);
	return prev;
}

bool LoggerSCM::do_logger_set_sync(bool enable)
{
	bool prev = enable;
	logger().set_sync_flag(enable);
	return prev;
}

bool LoggerSCM::do_logger_set_timestamp(bool enable)
{
	bool prev = enable;
	logger().set_timestamp_flag(enable);
	return prev;
}

bool LoggerSCM::do_logger_is_error_enabled()
{
	return logger().is_error_enabled();
}

bool LoggerSCM::do_logger_is_warn_enabled()
{
	return logger().is_warn_enabled();
}

bool LoggerSCM::do_logger_is_info_enabled()
{
	return logger().is_info_enabled();
}

bool LoggerSCM::do_logger_is_debug_enabled()
{
	return logger().is_debug_enabled();
}

bool LoggerSCM::do_logger_is_fine_enabled()
{
	return logger().is_fine_enabled();
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

void LoggerSCM::do_flush()
{
	logger().flush();
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

	define_scheme_primitive("cog-logger-set-component!",
		&LoggerSCM::do_logger_set_component, this, "logger");
	define_scheme_primitive("cog-logger-get-component",
		&LoggerSCM::do_logger_get_component, this, "logger");

	define_scheme_primitive("cog-logger-set-stdout!",
		&LoggerSCM::do_logger_set_stdout, this, "logger");
	define_scheme_primitive("cog-logger-set-sync!",
		&LoggerSCM::do_logger_set_sync, this, "logger");
	define_scheme_primitive("cog-logger-set-timestamp!",
		&LoggerSCM::do_logger_set_timestamp, this, "logger");

	define_scheme_primitive("cog-logger-error-enabled?",
		&LoggerSCM::do_logger_is_error_enabled, this, "logger");
	define_scheme_primitive("cog-logger-warn-enabled?",
		&LoggerSCM::do_logger_is_warn_enabled, this, "logger");
	define_scheme_primitive("cog-logger-info-enabled?",
		&LoggerSCM::do_logger_is_info_enabled, this, "logger");
	define_scheme_primitive("cog-logger-debug-enabled?",
		&LoggerSCM::do_logger_is_debug_enabled, this, "logger");
	define_scheme_primitive("cog-logger-fine-enabled?",
		&LoggerSCM::do_logger_is_fine_enabled, this, "logger");

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

	define_scheme_primitive("cog-logger-flush",
		&LoggerSCM::do_flush, this, "logger");
}

extern "C" {
void opencog_logger_init(void);
};

void opencog_logger_init(void)
{
    static LoggerSCM logger_scm;
    logger_scm.module_init();
}
