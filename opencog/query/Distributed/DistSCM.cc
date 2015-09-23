/*
 * DistSCM.cc
 *
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Mandeep Singh Bhatia , September 2015
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

#define MAX_SCM_CODE_STRING 5000

#include "DistSCM.h"
#include <opencog/guile/SchemeEval.h>//which lib to link?
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

bool DistSCM::master_mode=true;

DistSCM::DistSCM(void):true_string("true"),false_string("false")
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
  //blocking slave mode
	define_scheme_primitive("set-slave-mode", &DistSCM::slave_mode,
                            this, "dist-gearman");
  //exits the slave mode after last request is served
	define_scheme_primitive("set-master-mode", &DistSCM::set_master_mode,
                            this, "dist-gearman");
  //sends scheme string to slave and blocks till result arrives, returns uuid of resulting handle, invalid_uuid on failure
  define_scheme_primitive("dist-run-scm", &DistSCM::dist_scm,
                            this, "dist-gearman");
}

void DistSCM::set_master_mode(void)
{
	master_mode=true;
}

gearman_return_t DistSCM::worker_function(gearman_job_st *job, void *context)
{
  const char *workload= (const char *)gearman_job_workload(job);
  const size_t workload_size= gearman_job_workload_size(job);

  //std::cout << "Recieved " << workload_size << " bytes" << std::endl;
  if (workload_size>MAX_SCM_CODE_STRING+1) return GEARMAN_ERROR;
  char scm_str[MAX_SCM_CODE_STRING+1];
  strcpy(scm_str,workload);

  SchemeEval evl((AtomSpace*)context);
  evl.init_scheme();
  //std::cout<<scm_str<<"\n";
  Handle result=evl.eval_h(scm_str);//can we check if handle is valid? how to get current atomspace
  UUID vl=result.value();
  if (Handle::UNDEFINED==result)std::cerr<<"invalid handle \n";

  evl.eval("(sql-store)");

  if (gearman_failed(gearman_job_send_data(job, &vl, sizeof(vl))))
  {
		std::cerr<<"error occured";
    return GEARMAN_ERROR;//on gearman error return, no result is sent to master and job request stays in que
  }
   //std::cout<<"gearman data sent..."<<"\n";
   return GEARMAN_SUCCESS;
}

const std::string& DistSCM::slave_mode(const std::string& ip_string,const std::string& workerID)
{
	AtomSpace* atoms_ptr=SchemeSmob::ss_get_env_as("set-slave-mode");
	//std::cout<<"atomspace "<<atoms_ptr<<"\n";

    if ((worker= gearman_worker_create(NULL)) == NULL)
    {
      std::cerr << "Memory allocation failure on worker creation." << std::endl;
      return false_string;
    }

    gearman_worker_add_options(worker, GEARMAN_WORKER_GRAB_UNIQ);
    if (gearman_failed(gearman_worker_add_server(worker, ip_string.c_str(), GEARMAN_DEFAULT_TCP_PORT)))
    {
      std::cerr << gearman_worker_error(worker) << std::endl;
      return false_string;
    }
    if (gearman_failed(gearman_worker_set_identifier(worker, workerID.c_str(), workerID.length())))
    {
      std::cerr << gearman_worker_error(worker) << std::endl;
      return false_string;
    }
    gearman_function_t worker_fn= gearman_function_create(worker_function);
    if (gearman_failed(gearman_worker_define_function(worker,
                                                    gearman_literal_param("make_call"),
                                                    worker_fn,
                                                    0,
                                                    atoms_ptr)))
    {
      std::cerr << gearman_worker_error(worker) << std::endl;
      return false_string;
    }
	  gearman_worker_set_timeout(worker, 100);
	  master_mode=false;
    while(!(master_mode)){
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

UUID DistSCM::dist_scm(const std::string& scm_string,const std::string& clientID, bool truth)
{
  if (gearman_client_create(&client) == NULL)
  {
    std::cerr << "Memory allocation failure on client creation" << std::endl;
    return Handle::INVALID_UUID;
  }
  //
  int timeout=-1;
  //
  if (timeout >= 0)
  {
    gearman_client_set_timeout(&client, timeout);
  }

  gearman_return_t ret= gearman_client_add_server(&client, "localhost", GEARMAN_DEFAULT_TCP_PORT);
  if (ret != GEARMAN_SUCCESS)
  {
    std::cerr<<(gearman_client_error(&client));
    return Handle::INVALID_UUID;
  }
  if (gearman_failed(gearman_client_set_identifier(&client, clientID.c_str(), clientID.length())))
  {
    std::cerr<<(gearman_client_error(&client));
    return Handle::INVALID_UUID;
  }
  int exit_code= EXIT_SUCCESS;
  if (scm_string.length()>MAX_SCM_CODE_STRING)return Handle::INVALID_UUID;
  //int count=1; //if status is to be returned then don't change count on status receive
  UUID result_uuid;
    size_t result_size;
    char *result;
    char send_str[MAX_SCM_CODE_STRING+1];
    unsigned int send_len=0;
    strcpy(send_str,scm_string.c_str());
    send_len=scm_string.length()+1;
    result= (char *)gearman_client_do(&client, "make_call", NULL,
                                      send_str, send_len,
                                      &result_size, &ret);
    if (ret == GEARMAN_WORK_DATA)
    {
      memcpy(&result_uuid,result,result_size);//not wire safe if used on different endian systems
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
      memcpy(&result_uuid,result,result_size);//not wire safe if used on different endian systems
      free(result);
    }
    else if (ret == GEARMAN_WORK_FAIL)
    {
      std::cerr<<("Work failed");
      exit_code= EXIT_FAILURE;
    }
    else
    {
      std::cerr<<(gearman_client_error(&client));
      exit_code= EXIT_FAILURE;
    }

  gearman_client_free(&client);

  if (exit_code==EXIT_FAILURE)return Handle::INVALID_UUID;

	return result_uuid;
}

DistSCM::~DistSCM()
{

}

void opencog_dist_init(void)
{
	static DistSCM patty;
}
