/*
 * RandGenSCM.cc
 *
 * Copyright (C) 2017 OpenCog Foundation
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

#include <opencog/util/mt19937ar.h>
#include <opencog/guile/SchemeModule.h>
#include "../SchemePrimitive.h"

using namespace opencog;
namespace opencog {

/**
 * Expose opencog's RandGen singleton to Scheme
 */

class RandGenSCM : public ModuleWrap
{
protected:
	virtual void init();

	void do_randgen_set_seed(int);
	int do_randgen_randint(int);
	float do_randgen_randfloat();

public:
	RandGenSCM();
};


/// Set random seed
void RandGenSCM::do_randgen_set_seed(int s)
{
	randGen().seed(s);
}

/// Pick a random integer
int RandGenSCM::do_randgen_randint(int n)
{
	return randGen().randint(n);
}

/// Pick a random float in [0,1]
float RandGenSCM::do_randgen_randfloat()
{
	return randGen().randfloat();
}
	
} /*end of namespace opencog*/

RandGenSCM::RandGenSCM() : ModuleWrap("opencog randgen") {}

/// This is called while (opencog randgen) is the current module.
/// Thus, all the definitions below happen in that module.
void RandGenSCM::init(void)
{
	define_scheme_primitive("cog-randgen-set-seed!",
		&RandGenSCM::do_randgen_set_seed, this, "randgen");
	define_scheme_primitive("cog-randgen-randint",
		&RandGenSCM::do_randgen_randint, this, "randgen");
	define_scheme_primitive("cog-randgen-randfloat",
		&RandGenSCM::do_randgen_randfloat, this, "randgen");
}

extern "C" {
void opencog_randgen_init(void);
};

void opencog_randgen_init(void)
{
    static RandGenSCM randgen_scm;
    randgen_scm.module_init();
}
