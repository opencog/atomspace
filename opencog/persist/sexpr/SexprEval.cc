/**
 * SexprEval.cc
 *
 * S-expression evaluator.
 *
 * Copyright (c) 2008, 2014, 2015, 2020 Linas Vepstas
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
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/sexpr/Commands.h>

#include "SexprEval.h"

using namespace opencog;

// Single shared instance holding single shared frame cache.
Commands SexprEval::_interpreter;

SexprEval::SexprEval(AtomSpacePtr& asp)
	: GenericEval()
{
	_atomspace = asp;
}

SexprEval::~SexprEval()
{
}

/* ============================================================== */
/**
 * Evaluate an s-expresion.
 */
void SexprEval::eval_expr(const std::string &expr)
{
	_caught_error = false;
	try {
		std::lock_guard<std::mutex> lock(_mtx);
		_answer = _interpreter.interpret_command(
			(AtomSpace*) _atomspace.get(), expr);

		// CogStorageNode expects all responses to be terminated
		// by exactly one newline char. It is the end-of-message
		// marker.
		_answer += "\n";
	}
	catch (const StandardException& ex)
	{
		_error_string = ex.what();
		_caught_error = true;
	}
	catch (const std::runtime_error& ex)
	{
		_error_string = ex.what();
		_caught_error = true;
	}
}

std::string SexprEval::poll_result()
{
	std::string ret;
	std::lock_guard<std::mutex> lock(_mtx);
	ret.swap(_answer);
	return ret;
}


void SexprEval::begin_eval()
{
	std::lock_guard<std::mutex> lock(_mtx);
	while (0 < _answer.size())
	{
		logger().warn("This shouldn't happen!");
		usleep(100);
	}
	_answer.clear();
}

/* ============================================================== */

/**
 * interrupt() - convert user's control-C at keyboard into exception.
 */
void SexprEval::interrupt(void)
{
	_caught_error = true;
	_error_string = "Caught interrupt!";
}

SexprEval* SexprEval::get_evaluator(AtomSpacePtr& asp)
{
	static thread_local SexprEval* evaluator = new SexprEval(asp);

	// The eval_dtor runs when this thread is destroyed.
	class eval_dtor {
		public:
		~eval_dtor() { delete evaluator; }
	};
	static thread_local eval_dtor killer;

	return evaluator;
}

/* ===================== END OF FILE ======================== */
