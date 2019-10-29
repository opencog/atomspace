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
#include "../SchemePrimitive.h"

using namespace opencog;
namespace opencog {

/**
 * Expose the Logger singleton to Scheme
 */

class LoggerSCM : public ModuleWrap
{
protected:
	virtual void init();

	Logger* do_default_logger();
	Logger* do_new_logger();
	std::string do_logger_set_level(Logger*, const std::string& level);
	std::string do_logger_get_level(const Logger*);
	std::string do_logger_set_filename(Logger*, const std::string& filename);
	std::string do_logger_get_filename(const Logger*);
	std::string do_logger_set_component(Logger*, const std::string& component);
	std::string do_logger_get_component(const Logger*);
	bool do_logger_set_stdout(Logger*, bool);
	bool do_logger_set_sync(Logger*, bool);
	bool do_logger_set_timestamp(Logger*, bool);
	bool do_logger_is_error_enabled(Logger*);
	bool do_logger_is_warn_enabled(Logger*);
	bool do_logger_is_info_enabled(Logger*);
	bool do_logger_is_debug_enabled(Logger*);
	bool do_logger_is_fine_enabled(Logger*);
	void do_logger_error(Logger*, const std::string& msg);
	void do_logger_warn(Logger*, const std::string& msg);
	void do_logger_info(Logger*, const std::string& msg);
	void do_logger_debug(Logger*, const std::string& msg);
	void do_logger_fine(Logger*, const std::string& msg);
	void do_flush(Logger*);

	bool is_logger(SCM);

public:
	LoggerSCM();
};

/// Get the default logger.
Logger* LoggerSCM::do_default_logger()
{
	return &logger();
}

/// Create a new logger.
Logger* LoggerSCM::do_new_logger()
{
	return SchemeSmob::new_logger();
}

/// Set level, return previous level.
std::string LoggerSCM::do_logger_set_level(Logger* lg, const std::string& level)
{
	std::string prev_level;
	prev_level = Logger::get_level_string(lg->get_level());
	lg->set_level(Logger::get_level_from_string(level));
	return prev_level;
}

std::string LoggerSCM::do_logger_get_level(const Logger* lg)
{
	return Logger::get_level_string(lg->get_level());
}

/// Set logfile, return previous file.
std::string LoggerSCM::do_logger_set_filename(Logger* lg, const std::string& filename)
{
	std::string old_file;
	old_file = lg->get_filename();
	lg->set_filename(filename);
	return old_file;
}

std::string LoggerSCM::do_logger_get_filename(const Logger* lg)
{
	return lg->get_filename();
}

/// Set component, return previous component.
std::string LoggerSCM::do_logger_set_component(Logger* lg, const std::string& component)
{
	std::string old_component;
	old_component = lg->get_component();
	lg->set_component(component);
	return old_component;
}

std::string LoggerSCM::do_logger_get_component(const Logger* lg)
{
	return lg->get_component();
}

bool LoggerSCM::do_logger_set_stdout(Logger* lg, bool enable)
{
	// bool previous_setting = lg->get_print_to_stdout_flag();
	bool previous_setting = enable;
	lg->set_print_to_stdout_flag(enable);
	return previous_setting;
}

bool LoggerSCM::do_logger_set_sync(Logger* lg, bool enable)
{
	// bool previous_setting = lg->get_sync_flag();
	bool previous_setting = enable;
	lg->set_sync_flag(enable);
	return previous_setting;
}

bool LoggerSCM::do_logger_set_timestamp(Logger* lg, bool enable)
{
	// bool previous_setting = lg->get_timestamp_flag();
	bool previous_setting = enable;
	lg->set_timestamp_flag(enable);
	return previous_setting;
}

bool LoggerSCM::do_logger_is_error_enabled(Logger* lg)
{
	return lg->is_error_enabled();
}

bool LoggerSCM::do_logger_is_warn_enabled(Logger* lg)
{
	return lg->is_warn_enabled();
}

bool LoggerSCM::do_logger_is_info_enabled(Logger* lg)
{
	return lg->is_info_enabled();
}

bool LoggerSCM::do_logger_is_debug_enabled(Logger* lg)
{
	return lg->is_debug_enabled();
}

bool LoggerSCM::do_logger_is_fine_enabled(Logger* lg)
{
	return lg->is_fine_enabled();
}

void LoggerSCM::do_logger_error(Logger* lg, const std::string& msg)
{
	lg->error(msg);
}

void LoggerSCM::do_logger_warn(Logger* lg, const std::string& msg)
{
	lg->warn(msg);
}

void LoggerSCM::do_logger_info(Logger* lg, const std::string& msg)
{
	lg->info(msg);
}

void LoggerSCM::do_logger_debug(Logger* lg, const std::string& msg)
{
	lg->debug(msg);
}

void LoggerSCM::do_logger_fine(Logger* lg, const std::string& msg)
{
	lg->fine(msg);
}

void LoggerSCM::do_flush(Logger* lg)
{
	lg->flush();
}

bool LoggerSCM::is_logger(SCM s)
{
	return SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, s)
		and SCM_SMOB_FLAGS(s) == SchemeSmob::COG_LOGGER;
}

} /*end of namespace opencog*/

LoggerSCM::LoggerSCM() : ModuleWrap("opencog logger") {}

/// This is called while (opencog logger) is the current module.
/// Thus, all the definitions below happen in that module.
void LoggerSCM::init(void)
{
	define_scheme_primitive("cog-default-logger",
		&LoggerSCM::do_default_logger, this, "logger");
	define_scheme_primitive("cog-new-logger",
		&LoggerSCM::do_new_logger, this, "logger");

	define_scheme_primitive("cog-logger-set-level-of-logger!",
		&LoggerSCM::do_logger_set_level, this, "logger");
	define_scheme_primitive("cog-logger-get-level-of-logger",
		&LoggerSCM::do_logger_get_level, this, "logger");

	define_scheme_primitive("cog-logger-set-filename-of-logger!",
		&LoggerSCM::do_logger_set_filename, this, "logger");
	define_scheme_primitive("cog-logger-get-filename-of-logger",
		&LoggerSCM::do_logger_get_filename, this, "logger");

	define_scheme_primitive("cog-logger-set-component-of-logger!",
		&LoggerSCM::do_logger_set_component, this, "logger");
	define_scheme_primitive("cog-logger-get-component-of-logger",
		&LoggerSCM::do_logger_get_component, this, "logger");

	define_scheme_primitive("cog-logger-set-stdout-of-logger!",
		&LoggerSCM::do_logger_set_stdout, this, "logger");
	define_scheme_primitive("cog-logger-set-sync-of-logger!",
		&LoggerSCM::do_logger_set_sync, this, "logger");
	define_scheme_primitive("cog-logger-set-timestamp-of-logger!",
		&LoggerSCM::do_logger_set_timestamp, this, "logger");

	define_scheme_primitive("cog-logger-error-enabled-of-logger?",
		&LoggerSCM::do_logger_is_error_enabled, this, "logger");
	define_scheme_primitive("cog-logger-warn-enabled-of-logger?",
		&LoggerSCM::do_logger_is_warn_enabled, this, "logger");
	define_scheme_primitive("cog-logger-info-enabled-of-logger?",
		&LoggerSCM::do_logger_is_info_enabled, this, "logger");
	define_scheme_primitive("cog-logger-debug-enabled-of-logger?",
		&LoggerSCM::do_logger_is_debug_enabled, this, "logger");
	define_scheme_primitive("cog-logger-fine-enabled-of-logger?",
		&LoggerSCM::do_logger_is_fine_enabled, this, "logger");

	define_scheme_primitive("cog-logger-error-of-logger",
		&LoggerSCM::do_logger_error, this, "logger");
	define_scheme_primitive("cog-logger-warn-of-logger",
		&LoggerSCM::do_logger_warn, this, "logger");
	define_scheme_primitive("cog-logger-info-of-logger",
		&LoggerSCM::do_logger_info, this, "logger");
	define_scheme_primitive("cog-logger-debug-of-logger",
		&LoggerSCM::do_logger_debug, this, "logger");
	define_scheme_primitive("cog-logger-fine-of-logger",
		&LoggerSCM::do_logger_fine, this, "logger");

	define_scheme_primitive("cog-logger-flush-of-logger",
		&LoggerSCM::do_flush, this, "logger");

	define_scheme_primitive("cog-logger?",
		&LoggerSCM::is_logger, this, "logger");
}

extern "C" {
void opencog_logger_init(void);
};

void opencog_logger_init(void)
{
    static LoggerSCM logger_scm;
    logger_scm.module_init();
}
