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

#include <opencog/atoms/base/Handle.h>

#include <opencog/guile/SchemeEval.h>
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>
#include "DistSCM.h"

#define DEBUG 1
using namespace opencog;

bool DistSCM::master_mode = true;

DistSCM::DistSCM(void) : ModuleWrap("opencog dist-gearman")
{}

void DistSCM::init(void)
{
	// Blocking slave mode
	define_scheme_primitive("set-slave-mode", &DistSCM::slave_mode,
	                        this, "dist-gearman");
	// Exits the slave mode after last request is served
	define_scheme_primitive("set-master-mode", &DistSCM::set_master_mode,
	                        this, "dist-gearman");
	// Sends scheme string to slave and blocks till result arrives,
	// returns uuid of resulting handle, invalid_uuid on failure.
	define_scheme_primitive("dist-run-scm", &DistSCM::dist_scm,
							this, "dist-gearman");
}

void DistSCM::set_master_mode(void)
{
	master_mode = true;
}

gearman_return_t DistSCM::worker_function(gearman_job_st *job, void *context)
{
	const char *workload = (const char *)gearman_job_workload(job);

#ifdef DEBUG
	const size_t workload_size = gearman_job_workload_size(job);
	std::cout << "Dist worker recieved " << workload_size << " bytes" << std::endl;
	std::cout << "Dist worker job string is " << workload << std::endl;
#endif

	SchemeEval* evl = SchemeEval::get_evaluator((AtomSpace*) context);
	Handle result = evl->eval_h(workload);

#ifdef DEBUG
	std::cout << "Dist worker work result is " << result;
#endif

	UUID vl = result.value();
	if (gearman_failed(gearman_job_send_data(job, &vl, sizeof(vl))))
	{
		// On gearman error return, no result is sent to master and
		// job request stays in queue.
		std::cerr << " German worker: Error occured\n";
		return GEARMAN_ERROR;
	}
	return GEARMAN_SUCCESS;
}

std::string false_string = "Oh No, Mr. Billlll!";

const std::string& DistSCM::slave_mode(const std::string& ip_string,
                                       const std::string& workerID)
{
	AtomSpace* atoms_ptr = SchemeSmob::ss_get_env_as("set-slave-mode");

#ifdef DEBUG
	std::cout << "Dist set slave mode ip=" <<  ip_string
	          << " worker ID=" << workerID << std::endl;
#endif
	worker = gearman_worker_create(NULL);
	if (nullptr == worker)
	{
		std::cerr << "Memory allocation failure on worker creation." << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: Memory allocation failure on worker creation.");
	}

	gearman_worker_add_options(worker, GEARMAN_WORKER_GRAB_UNIQ);

	gearman_return_t rc;
	rc = gearman_worker_add_server(worker,
	                   ip_string.c_str(), GEARMAN_DEFAULT_TCP_PORT);

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
	rc = gearman_worker_define_function(worker,
	                                    gearman_literal_param("make_call"),
	                                    worker_fn,
	                                    0,
	                                    atoms_ptr);

	if (gearman_failed(rc))
	{
		std::cerr << gearman_worker_error(worker) << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_worker_error(worker));
	}

	gearman_worker_set_timeout(worker, 100);
	master_mode = false;
	while (not master_mode)
	{
		if (gearman_failed(gearman_worker_work(worker)))
		{
			std::cerr << gearman_worker_error(worker) << std::endl;
			gearman_worker_free(worker);
			return false_string;
		}
	}

	gearman_worker_free(worker);

	return ip_string;
}

UUID DistSCM::dist_scm(const std::string& scm_string,
                       const std::string& clientID, bool truth)
{
	gearman_client_st* clr;

	clr = gearman_client_create(&client);
	if (nullptr == clr)
	{
		std::cerr << "Memory allocation failure on client creation" << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: Memory allocation failure on client creation");
	}

	int timeout=-1;
	if (timeout >= 0)
	{
		gearman_client_set_timeout(&client, timeout);
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
	UUID result_uuid;
	size_t result_size;
	char* result = (char*) gearman_client_do(&client, "make_call", NULL,
	                              scm_string.c_str(), scm_string.length()+1,
	                              &result_size, &rc);
	if (rc == GEARMAN_WORK_DATA)
	{
		// XXX FIXME Not wire safe if used on different endian systems
		memcpy(&result_uuid, result, result_size);
		free(result);
	}
	else if (rc == GEARMAN_WORK_STATUS)
	{
		uint32_t numerator;
		uint32_t denominator;

		gearman_client_do_status(&client, &numerator, &denominator);
		std::clog << "Status: " << numerator << "/" << denominator << std::endl;
	}
	else if (rc == GEARMAN_SUCCESS)
	{
		// XXX FIXME Not wire safe if used on different endian systems
		memcpy(&result_uuid,result,result_size);
		free(result);
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

	if (exit_code == EXIT_FAILURE)
		return Handle::INVALID_UUID;

	return result_uuid;
}

DistSCM::~DistSCM()
{
}

void opencog_dist_init(void)
{
	static DistSCM dist_scm;
	dist_scm.module_init();
}
