/*
 * SchemeSmobDV.cc
 *
 * Scheme small objects (SMOBS) for attention values.
 *
 * Copyright (c) 2018 SingularityNet
 *
 * Written by Roman Treutlein <roman@singularitynet.io>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/truthvalue/DistributionalValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */
/**
 * Search for an distributional value in a list of values.
 * Return the attention value if found, else return null.
 * Throw errors if the list is not stictly just key-value pairs
 */
DistributionalValuePtr SchemeSmob::get_dv_from_list(SCM slist)
{
	while (scm_is_pair(slist))
	{
		SCM sval = SCM_CAR(slist);
		if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sval))
		{
			scm_t_bits misctype = SCM_SMOB_FLAGS(sval);
			switch (misctype)
			{
				case COG_PROTOM: {
					ProtoAtomPtr pa(scm_to_protom(sval));
					DistributionalValuePtr dv(DistributionalValueCast(pa));
					if (dv) return dv;
				}
				default:
					break;
			}
		}
		slist = SCM_CDR(slist);
	}
	return nullptr;
}

/* ============================================================== */

std::string SchemeSmob::dv_to_string(const DistributionalValuePtr& dv)
{
#define BUFLEN 120
	char buff[BUFLEN];

    throw RuntimeException(TRACE_INFO,"Not implemented.");

	// We pretend that they are actually short integers.
	//snprintf(buff, BUFLEN, "(dv %f %f %f)",
	//         dv->getSTI(), dv->getLTI(), dv->getVLTI());

	return buff;
}

/* ============================================================== */

SCM SchemeSmob::dv_to_scm (const DistributionalValuePtr& dv)
{
	return protom_to_scm(ProtoAtomCast(dv));
}

SCM SchemeSmob::dvs_to_scm(const std::vector<DistributionalValuePtr>& dvs)
{
	SCM res;
    bool first = true;
    for (DistributionalValuePtr dv : dvs)
    {
        SCM sdv = dv_to_scm(dv);
        if (first) {
            first = false;
            res = scm_list_1(sdv);
        }
        else {
            res = scm_cons(sdv,res);
        }
    }
	return res;
}

/**
 * Create a new distributional value based on a list of handles
 *                                        and a list of counts
 */
SCM SchemeSmob::ss_new_dv(SCM shs, SCM scs)
{
	ProtomSeq hs = verify_protom_list(shs,"cog-new-dv",1);
    std::vector<double> cs = scm_to_float_list(scs);
    auto it1 = hs.begin();
    auto it2 = cs.begin();
    auto end1 = hs.end();
    auto end2 = cs.end();
    ValueCounter hc;
    for (;(it1 != end1) && (it2 != end2); ++it1, ++it2)
    {
        hc[*it1] = *it2;
    }
	DistributionalValuePtr dv = DistributionalValue::createDV(hc);
	return dv_to_scm(dv);
}

SCM SchemeSmob::ss_new_dv_simple(SCM smean, SCM sconf)
{
	if (!scm_is_real(smean)) {
		scm_wrong_type_arg_msg("cog-new-dv-simple", 1, smean, "double");
	}
	if (!scm_is_real(sconf)) {
		scm_wrong_type_arg_msg("cog-new-dv-simple", 2, sconf, "double");
	}
	double mean = scm_to_double(smean);
	double conf = scm_to_double(sconf);
	DistributionalValuePtr dv = DistributionalValue::createDV(mean,conf);
	return dv_to_scm(dv);
}

/* ============================================================== */
/**
 * Return true if the scm is an distributional value
 */
SCM SchemeSmob::ss_dv_p (SCM s)
{
	ProtoAtomPtr pa(scm_to_protom(s));
	if (nullptr == pa) return SCM_BOOL_F;

	if (pa->get_type() == DISTRIBUTIONAL_VALUE)
		return SCM_BOOL_T;

	scm_remember_upto_here_1(s);
	return SCM_BOOL_F;
}

/* ============================================================== */

DistributionalValuePtr SchemeSmob::verify_dv(SCM sav, const char *subrname, int pos)
{
	ProtoAtomPtr pa(scm_to_protom(sav));
	DistributionalValuePtr dv(DistributionalValueCast(pa));

	if (nullptr == dv)
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog distributional value");

	return dv;
}

/**
 * Convert argument into a list of handles.
 */
std::vector<DistributionalValuePtr>
SchemeSmob::verify_dv_list (SCM sdv_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list
	if (!scm_is_pair(sdv_list))
		scm_wrong_type_arg_msg(subrname, pos, sdv_list, "a list of dvs");

    std::vector<DistributionalValuePtr> res;
	SCM sl = sdv_list;

	while (scm_is_pair(sl))
    {
		SCM sdv = SCM_CAR(sl);
        res.push_back(verify_dv(sdv,subrname,pos));
		sl = SCM_CDR(sl);
	}

	return res;
}

SCM SchemeSmob::ss_dv_divide(SCM sdv1,SCM sdv2,SCM si)
{
    DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-divide",1);
    DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-divide",2);
    int i = scm_to_int(si);
    ConditionalDVPtr cdv = dv1->divide(dv2,i);
    return cdv_to_scm(cdv);
}

SCM SchemeSmob::ss_dv_sum_joint(SCM sdv1,SCM sdv2,SCM si)
{
    DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-sum-joint",1);
    DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-sum-joint",2);
    int i = scm_to_int(si);
    DistributionalValuePtr dv3 = dv1->SumJoint(dv2,i);
    return dv_to_scm(dv3);
}

SCM SchemeSmob::ss_dv_part_joint(SCM sdv,SCM sh,SCM si)
{
    DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-part-joint",1);
    ProtoAtomPtr h = verify_protom(sh,"cog-dv-part-joint",2);
    int i = scm_to_int(si);
    DistributionalValuePtr dvres = DistributionalValue::createDV(dv->PartJoint(h,i));
    return dv_to_scm(dvres);
}

SCM SchemeSmob::ss_dv_conjunction(SCM sdv1,SCM sdv2)
{
    DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-conjunction",1);
    DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-conjunction",2);
	DistributionalValuePtr dvres = dv1->Conjuction(dv2);
	return dv_to_scm(dvres);
}

SCM SchemeSmob::ss_dv_disjunction(SCM sdv1,SCM sdv2)
{
    DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-disjunction",1);
    DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-disjunction",2);
	DistributionalValuePtr dvres = dv1->Disjuction(dv2);
	return dv_to_scm(dvres);
}

SCM SchemeSmob::ss_dv_get_swc(SCM sdv)
{
    DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-get-swc",1);
    return scm_from_double(dv->get_swc());
}

SCM SchemeSmob::ss_dv_get_fom(SCM sdv)
{
    DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-get-fom",1);
    return scm_from_double(dv->get_fstord_mean());
}

SCM SchemeSmob::ss_dv_get_confidence(SCM sdv)
{
    DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-get-confidence",1);
    return scm_from_double(dv->get_confidence());
}

SCM SchemeSmob::ss_dv_negate(SCM sdv)
{
    DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-negate",1);
    return dv_to_scm(dv->negate());
}

SCM SchemeSmob::cdv_to_scm (const ConditionalDVPtr& cdv)
{
	return protom_to_scm(ProtoAtomCast(cdv));
}

ConditionalDVPtr SchemeSmob::verify_cdv(SCM sav, const char *subrname, int pos)
{
	ProtoAtomPtr pa(scm_to_protom(sav));
	ConditionalDVPtr cdv(ConditionalDVCast(pa));

	if (nullptr == cdv)
		scm_wrong_type_arg_msg(subrname, pos, sav
                              , "opencog condtional distributional value");

	return cdv;
}

SCM SchemeSmob::ss_new_cdv(SCM sconds,SCM sdvs)
{
    ProtomSeq conds = verify_protom_list(sconds,"cog-new-cdv",1);
    std::vector<DistributionalValuePtr> dvs = verify_dv_list(sdvs,"cog-new-cdv",2);
    ConditionalDVPtr cdv = std::make_shared<ConditionalDV>(conds,dvs);
    return cdv_to_scm(cdv);
}

SCM SchemeSmob::ss_cdv_get_conditions(SCM scdv)
{
    ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-conditions",1);
    ProtomSeq conds = cdv->getConditions();
    return protomseq_to_scm(conds);
}

SCM SchemeSmob::ss_cdv_get_unconditonals(SCM scdv)
{
    ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-unconditionals",1);
    std::vector<DistributionalValuePtr> unconds = cdv->getUnconditionals();
    return dvs_to_scm(unconds);
}

SCM SchemeSmob::ss_cdv_get_unconditonal(SCM scdv,SCM sdv)
{
    ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-unconditional",1);
    DistributionalValuePtr dv = verify_dv(sdv,"cog-cdv-get-unconditional",2);
    DistributionalValuePtr res = cdv->getUnconditional(dv);
    return dv_to_scm(res);
}

SCM SchemeSmob::ss_cdv_get_unconditonal_no_match(SCM scdv,SCM sdv)
{
    ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-unconditional-no-match",1);
    DistributionalValuePtr dv = verify_dv(sdv,"cog-cdv-get-unconditional-no-match",2);
    DistributionalValuePtr res = cdv->getUnconditionalNoMatch(dv);
    return dv_to_scm(res);
}

SCM SchemeSmob::ss_cdv_get_joint(SCM scdv,SCM sdv)
{
   ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-joint",1);
   DistributionalValuePtr dv = verify_dv(sdv,"cog-cdv-get-joint",2);
   DistributionalValuePtr res = cdv->getJointProbability(dv);
   return dv_to_scm(res);
}

SCM SchemeSmob::ss_cdv_merge(SCM scdv1,SCM scdv2)
{
   ConditionalDVPtr cdv1 = verify_cdv(scdv1,"cog-cdv-merge",1);
   ConditionalDVPtr cdv2 = verify_cdv(scdv2,"cog-cdv-merge",2);
   ConditionalDVPtr res = cdv1->merge(cdv2);
   return cdv_to_scm(res);
}

SCM SchemeSmob::ss_cdv_cde(SCM scdv1,SCM scdv2)
{
   ConditionalDVPtr cdv1 = verify_cdv(scdv1,"cog-cdv-cde",1);
   ConditionalDVPtr cdv2 = verify_cdv(scdv2,"cog-cdv-cde",2);
   ConditionalDVPtr res = cdv1->CDE(cdv2);
   return cdv_to_scm(res);
}
/* ===================== END OF FILE ============================ */
