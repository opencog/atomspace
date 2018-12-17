/*
 * DistributionalValueSCMDV.cc
 *
 * Scheme small objects (SMOBS) for attention values.
 *
 * Copyright (c) 2018 SingularityNet
 *
 * Written by Roman Treutlein <roman@singularitynet.io>
 */

#include <opencog/guile/SchemePrimitive.h>
#include <opencog/guile/SchemeSmob.h>
#include <opencog/atoms/distvalue/DistributionalValueSCM.h>

using namespace opencog;

DistributionalValueSCM::DistributionalValueSCM()
	: ModuleWrap ("opencog distvalue") {}

void DistributionalValueSCM::init(void)
{
	define_scheme_primitive("cog-new-dv",
	                        &DistributionalValueSCM::ss_new_dv,
	                        this,"distvalue");
	define_scheme_primitive("cog-new-dv-simple",
	                        &DistributionalValueSCM::ss_new_dv_simple,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv?",
	                        &DistributionalValueSCM::ss_dv_p,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-divide",
	                        &DistributionalValueSCM::ss_dv_divide,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-sum-joint",
							&DistributionalValueSCM::ss_dv_sum_joint,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-get-confidence",
	                        &DistributionalValueSCM::ss_dv_get_confidence,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-conjunction",
	                        &DistributionalValueSCM::ss_dv_conjunction,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-disjunction",
	                        &DistributionalValueSCM::ss_dv_disjunction,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-negate",
	                        &DistributionalValueSCM::ss_dv_negate,
	                        this,"distvalue");
	define_scheme_primitive("cog-dv-is-empty",
	                        &DistributionalValueSCM::ss_dv_is_empty,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-is-empty",
	                        &DistributionalValueSCM::ss_cdv_is_empty,
	                        this,"distvalue");
	define_scheme_primitive("cog-new-cdv",
	                        &DistributionalValueSCM::ss_new_cdv,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-get-conditions",
	                        &DistributionalValueSCM::ss_cdv_get_conditions,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-get-unconditionals",
	                        &DistributionalValueSCM::ss_cdv_get_unconditonals,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-get-unconditional",
	                        &DistributionalValueSCM::ss_cdv_get_unconditonal,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-get-joint",
	                        &DistributionalValueSCM::ss_cdv_get_joint,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-merge",
	                        &DistributionalValueSCM::ss_cdv_merge,
	                        this,"distvalue");
	define_scheme_primitive("cog-cdv-cde",
	                        &DistributionalValueSCM::ss_cdv_cde,
	                        this,"distvalue");
}

//Utility Functions

Interval
DistributionalValueSCM::verify_interval(SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list.
	// Null lists are not valid intervals.
	if (!scm_is_pair(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a non-null list of float-pt values");
	return SchemeSmob::scm_to_float_list(svalue_list);
}

DVKey
DistributionalValueSCM::verify_DVKey(SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list.
	// Null lists are not valid intervals.
	if (!scm_is_pair(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a non-null list of float-pt values");
	return scm_to_DVKey(svalue_list);
}

DVKeySeq
DistributionalValueSCM::verify_DVKeySeq(SCM svalue_list, const char * subrname, int pos)
{
	// Verify that second arg is an actual list.
	// Null lists are not valid intervals.
	if (!scm_is_pair(svalue_list))
		scm_wrong_type_arg_msg(subrname, pos, svalue_list, "a non-null list of float-pt values");
	return scm_to_DVKeySeq(svalue_list);
}

DVKey DistributionalValueSCM::scm_to_DVKey(SCM svalue_list)
{
	DVKey valist;
	SCM sl = svalue_list;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			std::vector<double> v = verify_interval(svalue,"scm_to_DVKey",0);
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
	}
	return valist;
}

DVKeySeq DistributionalValueSCM::scm_to_DVKeySeq(SCM svalue_list)
{
	DVKeySeq valist;
	SCM sl = svalue_list;
	while (scm_is_pair(sl)) {
		SCM svalue = SCM_CAR(sl);

		if (not scm_is_null(svalue)) {
			DVKey v = verify_DVKey(svalue,"scm_to_DVKeySeq",0);
			valist.emplace_back(v);
		}
		sl = SCM_CDR(sl);
	}
	return valist;
}

SCM DistributionalValueSCM::float_list_to_scm(const std::vector<double>& v)
{
    SCM res = SCM_UNDEFINED;
    bool first = true;
    for (double d : v)
    {
        SCM sh = scm_from_double(d);
        if (first) {
            first = false;
            res = scm_list_1(sh);
        }
        else {
            res = scm_cons(sh,res);
        }
    }
    return res;
}

SCM DistributionalValueSCM::dvkey_to_scm(const DVKey& v)
{
    SCM res = SCM_UNDEFINED;
    bool first = true;
    for (auto d : v)
    {
        SCM sh = float_list_to_scm(d);
        if (first) {
            first = false;
            res = scm_list_1(sh);
        }
        else {
            res = scm_cons(sh,res);
        }
    }
    return res;
}

SCM DistributionalValueSCM::dvkeyseq_to_scm(const DVKeySeq& v)
{
    SCM res = SCM_UNDEFINED;
    bool first = true;
    for (auto d : v)
    {
        SCM sh = dvkey_to_scm(d);
        if (first) {
            first = false;
            res = scm_list_1(sh);
        }
        else {
            res = scm_cons(sh,res);
        }
    }
    return res;
}

/* ============================================================== */

std::string DistributionalValueSCM::dv_to_string(const DistributionalValuePtr& dv)
{
#define BUFLEN 120
	char buff[BUFLEN];

	throw RuntimeException(TRACE_INFO,"Not implemented.");

	// We pretend that they are actually short integers.
	//snprintf(buff, BUFLEN, "(dv %f %f %f)",
	//		   dv->getSTI(), dv->getLTI(), dv->getVLTI());

	return buff;
}

/* ============================================================== */

SCM DistributionalValueSCM::dv_to_scm (const DistributionalValuePtr& dv)
{
	return SchemeSmob::protom_to_scm(ValueCast(dv));
}

SCM DistributionalValueSCM::dvs_to_scm(const std::vector<DistributionalValuePtr>& dvs)
{
	SCM res = scm_list_n(SCM_UNDEFINED);
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
 *										  and a list of counts
 */
SCM DistributionalValueSCM::ss_new_dv(SCM sks, SCM scs)
{
	DVKeySeq ks = verify_DVKeySeq(sks,"cog-new-dv",1);
	std::vector<double> cs = SchemeSmob::scm_to_float_list(scs);
	auto it1 = ks.begin();
	auto it2 = cs.begin();
	auto end1 = ks.end();
	auto end2 = cs.end();
	DVCounter dvc;
	for (;(it1 != end1) && (it2 != end2); ++it1, ++it2)
	{
		dvc[*it1] = *it2;
	}
	DistributionalValuePtr dv = DistributionalValue::createDV(dvc);
	return dv_to_scm(dv);
}

SCM DistributionalValueSCM::ss_new_dv_simple(SCM smean, SCM sconf)
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
SCM DistributionalValueSCM::ss_dv_p (SCM s)
{
	ValuePtr pa(SchemeSmob::scm_to_protom(s));
	if (nullptr == pa) return SCM_BOOL_F;

	if (pa->get_type() == DISTRIBUTIONAL_VALUE)
		return SCM_BOOL_T;

	scm_remember_upto_here_1(s);
	return SCM_BOOL_F;
}

/* ============================================================== */

DistributionalValuePtr DistributionalValueSCM::verify_dv(SCM sav, const char *subrname, int pos)
{
	ValuePtr pa(SchemeSmob::scm_to_protom(sav));
	DistributionalValuePtr dv(DistributionalValueCast(pa));

	if (nullptr == dv)
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog distributional value");

	return dv;
}

/**
 * Convert argument into a list of handles.
 */
std::vector<DistributionalValuePtr>
DistributionalValueSCM::verify_dv_list (SCM sdv_list, const char * subrname, int pos)
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

SCM DistributionalValueSCM::ss_dv_divide(SCM sdv1,SCM sdv2,SCM si)
{
	DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-divide",1);
	DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-divide",2);
	int i = scm_to_int(si);
	ConditionalDVPtr cdv = DVFormulas::joint_to_cdv(dv1,dv2,i);
	return cdv_to_scm(cdv);
}

SCM DistributionalValueSCM::ss_dv_sum_joint(SCM sdv1,SCM si)
{
	DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-sum-joint",1);
	int i = scm_to_int(si);
	DistributionalValuePtr dvres = DVFormulas::sum_joint(dv1,i);
	return dv_to_scm(dvres);
}

SCM DistributionalValueSCM::ss_dv_conjunction(SCM sdv1,SCM sdv2)
{
	DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-conjunction",1);
	DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-conjunction",2);
	DistributionalValuePtr dvres = DVFormulas::conjunction(dv1,dv2);
	return dv_to_scm(dvres);
}

SCM DistributionalValueSCM::ss_dv_disjunction(SCM sdv1,SCM sdv2)
{
	DistributionalValuePtr dv1 = verify_dv(sdv1,"cog-dv-disjunction",1);
	DistributionalValuePtr dv2 = verify_dv(sdv2,"cog-dv-disjunction",2);
	DistributionalValuePtr dvres = DVFormulas::disjunction(dv1,dv2);
	return dv_to_scm(dvres);
}

SCM DistributionalValueSCM::ss_dv_get_fom(SCM sdv)
{
	DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-get-fom",1);
	return scm_from_double(dv->get_fstord_mean());
}

SCM DistributionalValueSCM::ss_dv_get_confidence(SCM sdv)
{
	DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-get-confidence",1);
	return scm_from_double(dv->get_confidence());
}

SCM DistributionalValueSCM::ss_dv_negate(SCM sdv)
{
	DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-negate",1);
	return dv_to_scm(dv->negate());
}

SCM DistributionalValueSCM::ss_dv_is_empty(SCM sdv)
{
	DistributionalValuePtr dv = verify_dv(sdv,"cog-dv-is-empty",1);
	return scm_from_bool(dv->value().size() == 0);
}

SCM DistributionalValueSCM::ss_cdv_is_empty(SCM scdv)
{
	ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-is-empty",1);
	return scm_from_bool(cdv->value().size() == 0);
}

SCM DistributionalValueSCM::cdv_to_scm (const ConditionalDVPtr& cdv)
{
	return SchemeSmob::protom_to_scm(ValueCast(cdv));
}

ConditionalDVPtr DistributionalValueSCM::verify_cdv(SCM sav, const char *subrname, int pos)
{
	ValuePtr pa(SchemeSmob::scm_to_protom(sav));
	ConditionalDVPtr cdv(ConditionalDVCast(pa));

	if (nullptr == cdv)
		scm_wrong_type_arg_msg(subrname, pos, sav
		                       , "opencog condtional distributional value");

	return cdv;
}

SCM DistributionalValueSCM::ss_new_cdv(SCM sconds,SCM sdvs)
{
	DVKeySeq conds = verify_DVKeySeq(sconds,"cog-new-cdv",1);
	std::vector<DistributionalValuePtr> dvs = verify_dv_list(sdvs,"cog-new-cdv",2);
	ConditionalDVPtr cdv = ConditionalDV::createCDV(conds,dvs);
	return cdv_to_scm(cdv);
}

SCM DistributionalValueSCM::ss_cdv_get_conditions(SCM scdv)
{
	ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-conditions",1);
	DVKeySeq conds = cdv->get_conditions();
	return dvkeyseq_to_scm(conds);
}

SCM DistributionalValueSCM::ss_cdv_get_unconditonals(SCM scdv)
{
	ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-unconditionals",1);
	std::vector<DistributionalValuePtr> unconds = cdv->get_unconditionals();
	return dvs_to_scm(unconds);
}

SCM DistributionalValueSCM::ss_cdv_get_unconditonal(SCM scdv,SCM sdv)
{
	ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-unconditional",1);
	DistributionalValuePtr dv = verify_dv(sdv,"cog-cdv-get-unconditional",2);
	DistributionalValuePtr res = cdv->get_unconditional(dv);
	return dv_to_scm(res);
}

SCM DistributionalValueSCM::ss_cdv_get_joint(SCM scdv,SCM sdv)
{
   ConditionalDVPtr cdv = verify_cdv(scdv,"cog-cdv-get-joint",1);
   DistributionalValuePtr dv = verify_dv(sdv,"cog-cdv-get-joint",2);
   DistributionalValuePtr res = cdv->get_joint_probability(dv);
   return dv_to_scm(res);
}

SCM DistributionalValueSCM::ss_cdv_merge(SCM scdv1,SCM scdv2)
{
   ConditionalDVPtr cdv1 = verify_cdv(scdv1,"cog-cdv-merge",1);
   ConditionalDVPtr cdv2 = verify_cdv(scdv2,"cog-cdv-merge",2);
   ConditionalDVPtr res = cdv1->merge(cdv2);
   return cdv_to_scm(res);
}

SCM DistributionalValueSCM::ss_cdv_cde(SCM scdv1,SCM scdv2)
{
   ConditionalDVPtr cdv1 = verify_cdv(scdv1,"cog-cdv-cde",1);
   ConditionalDVPtr cdv2 = verify_cdv(scdv2,"cog-cdv-cde",2);
   ConditionalDVPtr res = DVFormulas::consequent_disjunction_elemination(cdv1,cdv2);
   return cdv_to_scm(res);
}

void opencog_distvalue_init(void)
{
	static DistributionalValueSCM distvalueSCM;
	distvalueSCM.module_init();
}
/* ===================== END OF FILE ============================ */
