/*
 * opencog/atoms/distvalue/DistributionalValueSCM.h
 *
 * Copyright (c) 2018 by SingularityNet
 * All Rights Reserved
 *
 * Written by Roman Treutlein
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

#ifndef _OPENCOG_DISTRIBUTIONAL_VALUE_SCM_H
#define _OPENCOG_DISTRIBUTIONAL_VALUE_SCM_H

#include <opencog/atoms/distvalue/DistributionalValue.h>
#include <opencog/atoms/distvalue/ConditionalDV.h>
#include <opencog/atoms/distvalue/DVFormulas.h>

#include <opencog/guile/SchemeModule.h>

namespace opencog
{

class DistributionalValueSCM : public ModuleWrap
{

public:
	DistributionalValueSCM();

protected:
	void init(void);

	//C Value -> SCM
	SCM float_list_to_scm(const std::vector<double>&);
	SCM dvecseq_to_scm(const DVecSeq& v);
	SCM dv_to_scm(const DistributionalValuePtr&);
	SCM dvs_to_scm(const std::vector<DistributionalValuePtr>&);
	SCM cdv_to_scm(const ConditionalDVPtr&);

	//SCM -> C Value

	DVecSeq verify_DVecSeq(SCM svalue_list, const char * subrname, int pos);
	DVecSeq scm_to_DVecSeq(SCM svalue_list);
	DistributionalValuePtr verify_dv(SCM, const char *, int pos = 1);
	std::vector<DistributionalValuePtr> verify_dv_list(SCM, const char *,
	                                                   int pos = 1);
	ConditionalDVPtr verify_cdv(SCM, const char *, int pos = 1);

	//SCHEME Functions
	SCM ss_new_dv(SCM, SCM);
	SCM ss_new_dv_simple(SCM, SCM);
	SCM ss_dv_p(SCM);
	SCM ss_dv_divide(SCM,SCM,SCM);
	SCM ss_dv_sum_joint(SCM,SCM);
	SCM ss_dv_get_confidence(SCM);
	SCM ss_dv_conjunction(SCM,SCM);
	SCM ss_dv_disjunction(SCM,SCM);
	SCM ss_dv_negate(SCM);
	SCM ss_dv_is_empty(SCM);
	SCM ss_cdv_is_empty(SCM);
	SCM ss_new_cdv(SCM, SCM);
	SCM ss_cdv_get_conditions(SCM);
	SCM ss_cdv_get_unconditonals(SCM);
	SCM ss_cdv_get_unconditonal(SCM,SCM);
	SCM ss_cdv_get_joint(SCM,SCM);
	SCM ss_cdv_merge(SCM,SCM);
	SCM ss_cdv_join(SCM,SCM);
	//SCM ss_cdv_cde(SCM,SCM);

}; // class

} // namespace opencog

extern "C" {
void opencog_distvalue_init(void);
};

#endif // _OPENCOG_DISTRIBUTIONAL_VALUE_SCM_H
