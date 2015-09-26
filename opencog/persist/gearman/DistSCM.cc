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

#include <opencog/guile/SchemeEval.h>
#include <opencog/guile/SchemeSmob.h>
#include "DistSCM.h"

using namespace opencog;

bool DistSCM::master_mode = true;

DistSCM::DistSCM(void): true_string("true"), false_string("false")
{
	scm_with_guile(init_in_guile, this);
}

void* DistSCM::init_in_guile(void* self)
{
	scm_c_define_module("opencog dist-gearman", init_in_module, self);
	scm_c_use_module("opencog dist-gearman");
	return NULL;
}

void DistSCM::init_in_module(void* data)
{
	DistSCM* self = (DistSCM*) data;
	self->init();
}

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
	const char *workload= (const char *)gearman_job_workload(job);
	// const size_t workload_size= gearman_job_workload_size(job);

	//std::cout << "Recieved " << workload_size << " bytes" << std::endl;

	SchemeEval* evl = SchemeEval::get_evaluator((AtomSpace*) context);
	// Can we check if handle is valid? how to get current atomspace
	Handle result = evl->eval_h(workload);
	UUID vl = result.value();
	//evl->eval("(sql-store)");//store returned atom with store-atom, but let scheme code do it if required
	if (gearman_failed(gearman_job_send_data(job, &vl, sizeof(vl))))
	{
		// On gearman error return, no result is sent to master and
		// job request stays in queue.
  		std::cerr<<"error occured";
		return GEARMAN_ERROR;
	}
	//std::cout<<"gearman data sent..."<<"\n";
	return GEARMAN_SUCCESS;
}

const std::string& DistSCM::slave_mode(const std::string& ip_string,
                                       const std::string& workerID)
{
	AtomSpace* atoms_ptr=SchemeSmob::ss_get_env_as("set-slave-mode");
	//std::cout<<"atomspace "<<atoms_ptr<<"\n";

	if ((worker= gearman_worker_create(NULL)) == NULL)
	{
		std::cerr << "Memory allocation failure on worker creation." << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: Memory allocation failure on worker creation.");
	}

	gearman_worker_add_options(worker, GEARMAN_WORKER_GRAB_UNIQ);
	if (gearman_failed(gearman_worker_add_server(worker,
	                   ip_string.c_str(), GEARMAN_DEFAULT_TCP_PORT)))
	{
		std::cerr << gearman_worker_error(worker) << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_worker_error(worker));
	}

	if (gearman_failed(gearman_worker_set_identifier(worker,
                      workerID.c_str(), workerID.length())))
	{
		std::cerr << gearman_worker_error(worker) << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_worker_error(worker));
	}

	gearman_function_t worker_fn = gearman_function_create(worker_function);
	if (gearman_failed(gearman_worker_define_function(worker,
	                                       gearman_literal_param("make_call"),
	                                       worker_fn,
	                                       0,
	                                       atoms_ptr)))
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
			//always displays timeout
			////std::cerr << gearman_worker_error(worker) << std::endl;//comment these
			////return false_string;//when set with time out
		}
	}

	gearman_worker_free(worker);

	return ip_string;
}

UUID DistSCM::dist_scm(const std::string& scm_string,
                       const std::string& clientID, bool truth)
{
	if (gearman_client_create(&client) == NULL)
	{
		std::cerr << "Memory allocation failure on client creation" << std::endl;
		throw RuntimeException(TRACE_INFO,
			"Gearman: Memory allocation failure on client creation");
	}
	//
	int timeout=-1;
	//
	if (timeout >= 0)
	{
		gearman_client_set_timeout(&client, timeout);
	}

	gearman_return_t ret= gearman_client_add_server(&client,
                              "localhost", GEARMAN_DEFAULT_TCP_PORT);
	if (ret != GEARMAN_SUCCESS)
	{
		std::cerr << gearman_client_error(&client);
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_client_error(&client));
	}

	if (gearman_failed(gearman_client_set_identifier(&client,
                               clientID.c_str(), clientID.length())))
	{
		std::cerr<<(gearman_client_error(&client));
		throw RuntimeException(TRACE_INFO,
			"Gearman: %s", gearman_client_error(&client));
	}

	int exit_code = EXIT_SUCCESS;
	UUID result_uuid;
	size_t result_size;
	char* result = (char*) gearman_client_do(&client, "make_call", NULL,
	                              scm_string.c_str(), scm_string.length()+1,
	                              &result_size, &ret);
	if (ret == GEARMAN_WORK_DATA)
	{
		// XXX FIXME Not wire safe if used on different endian systems
		memcpy(&result_uuid, result, result_size);
		free(result);
	}
	else if (ret == GEARMAN_WORK_STATUS)
	{
		uint32_t numerator;
		uint32_t denominator;

		gearman_client_do_status(&client, &numerator, &denominator);
		std::clog << "Status: " << numerator << "/" << denominator << std::endl;
	}
	else if (ret == GEARMAN_SUCCESS)
	{
		// XXX FIXME Not wire safe if used on different endian systems
		memcpy(&result_uuid,result,result_size);
		free(result);
	}
	else if (ret == GEARMAN_WORK_FAIL)
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
	static DistSCM patty;
}
