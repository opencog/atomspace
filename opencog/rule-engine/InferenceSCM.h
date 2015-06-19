/*
 * InferenceSCM.h
 *
 * Copyright (C) 2014 Misgana Bayetta
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>  Sept 2014
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

#ifndef _OPENCOG_INFERENCE_SCM_H
#define _OPENCOG_INFERENCE_SCM_H

#include <opencog/atomspace/Handle.h>

namespace opencog {

class InferenceSCM
{
private:
	static void* init_in_guile(void*);
	static void init_in_module(void*);

	void init(void);

	// 
	/**
	 * Run Forward Chaining on source h and rule-based system rbs
	 *
	 * @param h target
	 * @param rbs rule-based system atom
	 * @return ???
	 */
	Handle do_forward_chaining(Handle h, Handle rbs);
	/**
	 * @param h target
	 * @param rbs rule-based system atom
	 * @return ???
	 */
	Handle do_backward_chaining(Handle h, Handle rbs);
public:
	InferenceSCM();
};

} /*end of namespace opencog*/

extern "C" {
void opencog_ruleengine_init(void);
};

#endif /* _OPENCOG_INFERENCE_SCM_H */
