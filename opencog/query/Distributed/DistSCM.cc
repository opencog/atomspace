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
#include "DistSCM.h"
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
static gearman_return_t worker_function(gearman_job_st *job, void *context)
{
  const char *workload= (const char *)gearman_job_workload(job);
  const size_t workload_size= gearman_job_workload_size(job);

  std::cout << "Recieved " << workload_size << " bytes" << std::endl;
////  run eval_h(""), get handle, wrap to anchor node and push to db
  char *result=new char[workload_size];
  memcpy(result,workload,workload_size);
  
  /*
   *if (gearman_failed(gearman_job_send_data(job, &result[y], 1)))
      {
        return GEARMAN_ERROR;
      }
   */
    if (gearman_failed(gearman_job_send_data(job, &result[0], workload_size)))
    {
      return GEARMAN_ERROR;
    }
   
}

const std::string& DistSCM::slave_mode(const std::string& ip_string,const std::string& workerID)
{
    if ((worker= gearman_worker_create(NULL)) == NULL)
    {
      std::cerr << "Memory allocation failure on worker creation." << std::endl;
      return false_string;
    }
    gearman_worker_add_options(worker, GEARMAN_WORKER_GRAB_UNIQ);
    gearman_worker_set_timeout(worker, 200);//ms
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
                                                    NULL/*&options*/)))
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
const std::string& DistSCM::dist_scm(const std::string& scm_string)
{
	return true_string;
}
DistSCM::~DistSCM()
{
	
}
void opencog_dist_init(void)
{
	static DistSCM patty;
}
