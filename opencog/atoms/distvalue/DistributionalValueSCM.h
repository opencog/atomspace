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

namespace opencog
{

class DistributionalValueSCM
{

	static SCM dv_to_scm(const DistributionalValuePtr&);

	static SCM dvs_to_scm(const std::vector<DistributionalValuePtr>&);
	static SCM cdv_to_scm(const ConditionalDVPtr&);
	static SCM float_list_to_scm(const std::vector<double>&);
	static SCM dvkey_to_scm(const DVKey&);
	static SCM dvkeyseq_to_scm(const DVKeySeq&);

	static SCM ss_new_dv(SCM, SCM);
	static SCM ss_new_dv_simple(SCM, SCM);
	static SCM ss_dv_p(SCM);
	static SCM ss_dv_divide(SCM,SCM,SCM);
	static SCM ss_dv_sum_joint(SCM,SCM);
	static SCM ss_dv_get_confidence(SCM);
	static SCM ss_dv_get_fom(SCM);
	static SCM ss_dv_conjunction(SCM,SCM);
	static SCM ss_dv_disjunction(SCM,SCM);
	static SCM ss_dv_negate(SCM);
	static SCM ss_dv_is_empty(SCM);
	static SCM ss_cdv_is_empty(SCM);
	static SCM ss_new_cdv(SCM, SCM);
	static SCM ss_cdv_get_conditions(SCM);
	static SCM ss_cdv_get_unconditonals(SCM);
	static SCM ss_cdv_get_unconditonal(SCM,SCM);
	static SCM ss_cdv_get_joint(SCM,SCM);
	static SCM ss_cdv_merge(SCM,SCM);
	static SCM ss_cdv_cde(SCM,SCM);

	static DVKey verify_DVKey(SCM, const char *, int pos=1);
	static DVKeySeq verify_DVKeySeq(SCM, const char *, int pos=1);
	static DistributionalValuePtr verify_dv(SCM, const char *, int pos = 1);
	static std::vector<DistributionalValuePtr> verify_dv_list(SCM, const char *,
	                                                          int pos = 1);
	static ConditionalDVPtr verify_cdv(SCM, const char *, int pos = 1);

}; // class

#endif // _OPENCOG_DISTRIBUTIONAL_VALUE_SCM_H
