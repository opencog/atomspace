/*
 * DistSCM.cc
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Mandeep Singh Bhatia, September 2015
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

#include <string>
#include <libgearman/gearman.h>

#include <opencog/guile/SchemeEval.h>
#include <opencog/guile/SchemeModule.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

namespace opencog {

class DistSCM : public ModuleWrap
{
private:
	void init(void);

	/// All threads will keep handling work until this is set to false.
	static bool keep_working;
	void exit_all_workers(void);

	std::string start_work_handler(const std::string& ipaddr_string,
	                               const std::string& workerID);
	std::string dist_eval(const std::string& work_string,
	                      const std::string& clientID);

	// XXX FIXME -- a single client and worker? This cannot be right!
	gearman_client_st client;
	gearman_worker_st *worker;
	static gearman_return_t worker_function(gearman_job_st *job, void *context);

public:
	DistSCM(void);
	~DistSCM();
};

} // namespace opencog

#define DEBUG 1
using namespace opencog;

bool DistSCM::keep_working = true;

DistSCM::DistSCM(void) :
    ModuleWrap("opencog dist-gearman"),
    worker(nullptr)
{}

void DistSCM::init(void)
{
	// Enter Blocking slave mode
	define_scheme_primitive("start-work-handler", &DistSCM::start_work_handler,
	                        this, "dist-gearman");

	// Exits the slave mode after last request is served
	define_scheme_primitive("exit-all-workers", &DistSCM::exit_all_workers,
	                        this, "dist-gearman");

	// Sends scheme string to worker and block until a result arrives.
	// Returns resulting scheme string.
	define_scheme_primitive("dist-eval", &DistSCM::dist_eval,
	                        this, "dist-gearman");
}

/// This method causes all worker threads to return to their callers
/// after completing whatever work they are doing.
void DistSCM::exit_all_workers(void)
{
	keep_working = false;
}

gearman_return_t DistSCM::worker_function(gearman_job_st *job, void *context)
{
	const char *workload = (const char *)gearman_job_workload(job);

#ifdef DEBUG
	const size_t workload_size = gearman_job_workload_size(job);
	std::cout << "Dist worker received " << workload_size << " bytes" << std::endl;
	std::cout << "Dist worker job string is " << workload << std::endl;
#endif

	// All worker threads are sharing the same atomspace.
	// This seems like the right thing to do, I guess.
	AtomSpace* atomspace = (AtomSpace*) context;
	SchemeEval* evl = SchemeEval::get_evaluator(atomspace);
	Handle result = evl->eval_h(workload);

#ifdef DEBUG
	std::cout << "Dist worker work result is " << result;
#endif

	std::string reply = result->to_string();
	gearman_return_t rc = gearman_job_send_data(job, reply.c_str(), reply.length());

	if (gearman_failed(rc))
	{
		// On gearman error return, no result is sent to the client,
		// and the job request stays in queue.
		std::cerr << "Error: Gearman worker: Unable to send result\n";
		return GEARMAN_ERROR;
	}
	return GEARMAN_SUCCESS;
}

std::string false_string = "Oh Noooo, Mr. Bill!";

/// This method causes the current thread to enter an infinite
/// loop, waiting for job requests from clients.  This method
/// will not return, unless there is a Gearman error, or unless
/// the exit_all_handlers() method is called.
///
/// `ipaddr_string` is the IP address of the gearmand server to contact.
/// The default port number will be used.
std::string DistSCM::start_work_handler(const std::string& ipaddr_string,
                                        const std::string& workerID)
{
#ifdef DEBUG
	std::cout << "Dist start-work-handler ip=" <<  ipaddr_string
	          << " worker ID=" << workerID << std::endl;
#endif
	worker = gearman_worker_create(nullptr);
	if (nullptr == worker)
	{
		std::cerr << "Memory allocation failure on worker creation." << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: Memory allocation failure on worker creation.");
	}

	gearman_worker_add_options(worker, GEARMAN_WORKER_GRAB_UNIQ);

	gearman_return_t rc;
	rc = gearman_worker_add_server(worker, ipaddr_string.c_str(),
	                               GEARMAN_DEFAULT_TCP_PORT);

	if (gearman_failed(rc))
	{
		std::cerr << gearman_worker_error(worker) << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_worker_error(worker));
	}

	rc = gearman_worker_set_identifier(worker,
                      workerID.c_str(), workerID.length());

	if (gearman_failed(rc))
	{
		std::cerr << gearman_worker_error(worker) << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_worker_error(worker));
	}

	gearman_function_t worker_fn = gearman_function_create(worker_function);

	// Get the atomspace for this thread.
	AtomSpace* atomspace = SchemeSmob::ss_get_env_as("start-work-handler");

	rc = gearman_worker_define_function(worker,
	                                    gearman_literal_param("make_call"),
	                                    worker_fn,
	                                    0,
	                                    atomspace);

	if (gearman_failed(rc))
	{
		std::cerr << gearman_worker_error(worker) << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_worker_error(worker));
	}

#ifdef DEBUG
	std::cout << "Dist start-work-handler enter main loop\n";
#endif
	// Timeout is in milliseconds. Set it to 1000 millsecs.
	// As a result, we will poll, once a second, and then
	// pop to back to the main loop, to see if we should halt,
	// or not.
	gearman_worker_set_timeout(worker, 1000);
	while (keep_working)
	{
		gearman_return_t rc = gearman_worker_work(worker);
		if (rc == GEARMAN_TIMEOUT) continue;

		if (gearman_failed(rc))
		{
			std::cerr << "Dist error: " << gearman_worker_error(worker) << std::endl;
			gearman_worker_free(worker);
			return false_string;
		}
	}

	gearman_worker_free(worker);

	return ipaddr_string;
}

/// Send the string `scm_string` (assumed to be a valid scheme expression)
/// to the gearmand server.  Wait for a reply, which will be (should be)
/// a valid scheme expression.
std::string DistSCM::dist_eval(const std::string& scm_string,
                               const std::string& clientID)
{
	gearman_client_st* clr;

	clr = gearman_client_create(&client);
	if (nullptr == clr)
	{
		std::cerr << "Memory allocation failure on client creation" << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: Memory allocation failure on client creation");
	}

	gearman_return_t rc = gearman_client_add_server(&client,
                              "localhost", GEARMAN_DEFAULT_TCP_PORT);
	if (rc != GEARMAN_SUCCESS)
	{
		std::cerr << gearman_client_error(&client);
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_client_error(&client));
	}

	rc = gearman_client_set_identifier(&client,
                               clientID.c_str(), clientID.length());
	if (gearman_failed(rc))
	{
		std::cerr << gearman_client_error(&client);
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_client_error(&client));
	}

	int exit_code = EXIT_SUCCESS;
	size_t result_size;
	char* result = (char*) gearman_client_do(&client, "make_call", NULL,
	                              scm_string.c_str(), scm_string.length()+1,
	                              &result_size, &rc);
	std::string work_result;
	if (rc == GEARMAN_WORK_DATA or rc == GEARMAN_SUCCESS)
	{
		work_result = result;
		free(result);
	}
	else if (rc == GEARMAN_WORK_STATUS)
	{
		uint32_t numerator;
		uint32_t denominator;

		gearman_client_do_status(&client, &numerator, &denominator);
		std::clog << "Status: " << numerator << "/" << denominator << std::endl;
	}
	else if (rc == GEARMAN_WORK_FAIL)
	{
		std::cerr << "Work failed";
		exit_code = EXIT_FAILURE;
	}
	else
	{
		std::cerr << gearman_client_error(&client);
		exit_code = EXIT_FAILURE;
	}

	gearman_client_free(&client);

	if (exit_code == EXIT_FAILURE) return "";

	return work_result;
}

DistSCM::~DistSCM()
{
}

extern "C" {
void opencog_dist_init(void);
};

void opencog_dist_init(void)
{
	static DistSCM gearman_dist;
	gearman_dist.module_init();
}
