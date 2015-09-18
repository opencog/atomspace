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
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
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
#ifdef HAVE_GUILE
	define_scheme_primitive("set-slave-mode", &DistSCM::slave_mode,
                            this, "dist-gearman");
                            //if master ip is empty string then make this a slave
	define_scheme_primitive("set-master-mode", &DistSCM::set_master_mode,
                            this, "dist-gearman");

    define_scheme_primitive("dist-run-scm", &DistSCM::dist_scm,
                            this, "dist-gearman");
    //there will be a slave-run-scm non scheme primitive gearman worker active if is set as a slave
#endif
}
void DistSCM::set_master_mode(void)
{
	master_mode=true;
}

gearman_return_t DistSCM::worker_function(gearman_job_st *job, void *context)
{
  const char *workload= (const char *)gearman_job_workload(job);
  const size_t workload_size= gearman_job_workload_size(job);

  std::cout << "Recieved " << workload_size << " bytes" << std::endl;
////  run eval_h(""), get handle, wrap to anchor node and push to db
  //char *result=new char[workload_size];
  //memcpy(result,workload,workload_size);
  char scm_str[MAX_SCM_CODE_STRING+1];
  strcpy(scm_str,workload);
  unsigned int slen=strlen(scm_str);
  char tag[16];
  memcpy(&tag,scm_str+slen+1, 16);
  SchemeEval evl((AtomSpace*)context);
  evl.init_scheme();
  Handle result=evl.eval_h(scm_str);//can we check if handle is valid? how to get current atomspace
  //wrap handle with a custom link and ...
  //push to backing store
  
    if (gearman_failed(gearman_job_send_data(job, "true", strlen("true"))))
    {
      return GEARMAN_ERROR;
    }
   return GEARMAN_SUCCESS;
}

const std::string& DistSCM::slave_mode(const std::string& ip_string,const std::string& workerID)
{
	AtomSpace* atoms_ptr=SchemeSmob::ss_get_env_as("set_slave_mode");
	
    if ((worker= gearman_worker_create(NULL)) == NULL)
    {
      std::cerr << "Memory allocation failure on worker creation." << std::endl;
      return false_string;
    }
    
    gearman_worker_add_options(worker, GEARMAN_WORKER_GRAB_UNIQ);
    gearman_worker_set_timeout(worker, 100);//ms
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

	master_mode=false;
    while(!(master_mode)){
    if (gearman_failed(gearman_worker_work(worker)))
    {
      std::cerr << gearman_worker_error(worker) << std::endl;
      return false_string;
    }
	}
  
	//master_ip=ip_string;
	gearman_worker_free(worker);
	
	return ip_string;
}
const std::string& DistSCM::dist_scm(const std::string& scm_string,const std::string& clientID)
{ //max scheme code = 4900 bytes
  if (gearman_client_create(&client) == NULL)
  {
    std::cerr << "Memory allocation failure on client creation" << std::endl;
    return false_string;
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
    return false_string;
  }
  if (gearman_failed(gearman_client_set_identifier(&client, clientID.c_str(), clientID.length())))
  {
    std::cerr<<(gearman_client_error(&client));
    return false_string;
  }
  int exit_code= EXIT_SUCCESS;
  if (scm_string.length()>MAX_SCM_CODE_STRING)return false_string;
  int count=1; //if status is to be returned then don't change count on status receive
  do
  {
    size_t result_size;
    char *result;
    char send_str[MAX_SCM_CODE_STRING+1+16];
    unsigned int send_len=0;
    strcpy(send_str,scm_string.c_str());
    send_len+=scm_string.length()+1;
    boost::uuids::uuid tag=boost::uuids::random_generator()();
    memcpy(send_str+send_len,&tag, 16);
    send_len+=16;
    //send_str = scm string + '\0' + uuid
    result= (char *)gearman_client_do(&client, "make_call", NULL,
                                      send_str, send_len,
                                      &result_size, &ret);
    if (ret == GEARMAN_WORK_DATA)
    {
      std::cout.write(result, result_size);

      free(result);
      continue;
    }
    else if (ret == GEARMAN_WORK_STATUS)
    {
      uint32_t numerator;
      uint32_t denominator;

      gearman_client_do_status(&client, &numerator, &denominator);
      std::clog << "Status: " << numerator << "/" << denominator << std::endl;
      continue;
    }
    else if (ret == GEARMAN_SUCCESS)
    {
      std::cout.write(result, result_size);
      free(result);
    }
    else if (ret == GEARMAN_WORK_FAIL)
    {
      std::cerr<<("Work failed");
      exit_code= EXIT_FAILURE;
      break;
    }
    else
    {
      std::cerr<<(gearman_client_error(&client));
      exit_code= EXIT_FAILURE;
      break;
    }

    --count;

  } while (count);

  gearman_client_free(&client);

  if (exit_code==EXIT_FAILURE)return false_string;
	
	return true_string;
}
DistSCM::~DistSCM()
{
	
}
void opencog_dist_init(void)
{
	static DistSCM patty;
}
