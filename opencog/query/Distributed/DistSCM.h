/*
 * DistSCM.h
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
#include <string>

#include <libgearman/gearman.h>

//#include <opencog/guile/SchemeModule.h> //function wrap not defined for dist function
//taken from InferenceSCM
#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>

//
#include <opencog/atomspace/Handle.h>


//(use-modules (opencog gearman))
//(set-slave-mode "mapster-ip" "worker1")
namespace opencog {

class DistSCM 
{
private:
	static void* init_in_guile(void*);
	static void init_in_module(void*);

	void init(void);
	static bool master_mode;
	void set_master_mode(void);
	const std::string& slave_mode(const std::string& ip_string,const std::string& workerID);
	UUID dist_scm(const std::string& scm_string,const std::string& clientID,bool truth);
	const std::string true_string, false_string;
	std::string master_ip;

	gearman_client_st client;
	gearman_worker_st *worker;
	static gearman_return_t worker_function(gearman_job_st *job, void *context);
	
	public:
		DistSCM(void);
		~DistSCM();
};

}

extern "C" {
void opencog_dist_init(void);
};
