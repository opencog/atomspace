/*
 * SchemeSmobTV.c
 *
 * Scheme small objects (SMOBS) for truth values.
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linas@linas.org>
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

#include <cstddef>
#include <libguile.h>

#include <opencog/truthvalue/FuzzyTruthValue.h>
#include <opencog/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/truthvalue/CountTruthValue.h>
#include <opencog/truthvalue/IndefiniteTruthValue.h>
#include <opencog/truthvalue/SimpleTruthValue.h>
#include <opencog/truthvalue/EvidenceCountTruthValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */

#ifdef USE_KEYWORD_LIST_NOT_USED
/**
 * Search for a truth value (demarked by #:tv) in a list of key-value
 * pairs.  Return the truth value if found, else return null.
 * Throw errors if the list is not stictly just key-value pairs
 *
 * XXX This code is not currently used, since it seems pointless
 * to have key-value pairs for this function. After all, an atom
 * can only have one truth value ever -- if we find a truth value, we
 * use it. We don't really need a key to tell us that its a truth value.
 * So punt, and get truth values implicitly. Meanwhile, this code is
 * stubbed out, for a rainy tay, in case we need to resurrect key-value
 * pairs in the future.
 */
static TruthValue *get_tv_from_kvp(SCM kvp, const char * subrname, int pos)
{
	if (!scm_is_pair(kvp)) return NULL;

	do
	{
		SCM skey = SCM_CAR(kvp);

		// Verify that the first item is a keyword.
		if (!scm_is_keyword(skey))
			scm_wrong_type_arg_msg(subrname, pos, skey, "keyword");

		skey = scm_keyword_to_symbol(skey);
		skey = scm_symbol_to_string(skey);
		char * key = scm_to_utf8_string(skey);

		kvp = SCM_CDR(kvp);
		pos ++;
		if (!scm_is_pair(kvp))
		{
			scm_wrong_type_arg_msg(subrname, pos, kvp, "value following keyword");
		}

		if (0 == strcmp(key, "tv"))
		{
			SCM sval = SCM_CAR(kvp);
			scm_t_bits misctype = SCM_SMOB_FLAGS(sval);
			if (misctype != COG_SIMPLE_TV)
				scm_wrong_type_arg_msg(subrname, pos, sval, "opencog truth value");
			TruthValue *tv;
			tv = (TruthValue *) SCM_SMOB_DATA(sval);
			return tv;
		}
		free(key);

		kvp = SCM_CDR(kvp);
		pos ++;
	}
	while (scm_is_pair(kvp));

	return NULL;
}
#endif /* USE_KEYWORD_LIST_NOT_USED */

/**
 * Search for a truth value in a list of values.
 * Return the truth value if found, else return null.
 * Throw errors if the list is not strictly just key-value pairs
 */
TruthValuePtr SchemeSmob::get_tv_from_list(SCM slist)
{
	while (scm_is_pair(slist))
	{
		SCM sval = SCM_CAR(slist);
		if (SCM_SMOB_PREDICATE(SchemeSmob::cog_misc_tag, sval))
		{
			scm_t_bits misctype = SCM_SMOB_FLAGS(sval);
			switch (misctype)
			{
				case COG_PROTOM:
					ProtoAtomPtr pa(scm_to_protom(sval));
					return TruthValueCast(pa);
				default:
					break;
			}
		}

		slist = SCM_CDR(slist);
	}

	return nullptr;
}

/* ============================================================== */

std::string SchemeSmob::tv_to_string(const TruthValuePtr& tv)
{
#define BUFLEN 120
	char buff[BUFLEN];
	Type tvt = tv->getType();

	// Pretend they're floats, not doubles, so print with 8 digits
	std::string ret = "";
	if (SIMPLE_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(stv %.8g ", tv->getMean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->getConfidence());
		ret += buff;
		return ret;
	}

	if (COUNT_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(ctv %.8g ", tv->getMean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g ", tv->getConfidence());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->getCount());
		ret += buff;
		return ret;
	}

	if (INDEFINITE_TRUTH_VALUE == tvt)
	{
		const IndefiniteTruthValuePtr itv = static_cast<IndefiniteTruthValuePtr>(tv);
		snprintf(buff, BUFLEN, "(itv %.8g ", itv->getL());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g ", itv->getU());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", itv->getConfidenceLevel());
		ret += buff;
		return ret;
	}

	if (PROBABILISTIC_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(ptv %.8g ", tv->getMean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g ", tv->getConfidence());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->getCount());
		ret += buff;
		return ret;
	}

	if (FUZZY_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(ftv %.8g ", tv->getMean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->getConfidence());
		ret += buff;
		return ret;
	}

	if (EVIDENCE_COUNT_TRUTH_VALUE == tvt)
	{
		const EvidenceCountTruthValuePtr etv = static_cast<EvidenceCountTruthValuePtr>(tv);
		snprintf(buff, BUFLEN, "(etv %.8g ", etv->getPositiveCount());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", etv->getCount());
		ret += buff;
		return ret;
	}
	return ret;
}

/* ============================================================== */
/**
 * Create a new simple truth value, with indicated mean and confidence.
 */
SCM SchemeSmob::ss_new_stv (SCM smean, SCM sconfidence)
{
	double mean = scm_to_double(smean);
	double confidence = scm_to_double(sconfidence);

	ProtoAtomPtr tv = new SimpleTruthValue(mean, confidence);
	return protom_to_scm(tv);
}

SCM SchemeSmob::ss_new_ctv (SCM smean, SCM sconfidence, SCM scount)
{
	double mean = scm_to_double(smean);
	double confidence = scm_to_double(sconfidence);
	double count = scm_to_double(scount);

	ProtoAtomPtr tv = new CountTruthValue(mean, confidence, count);
	return protom_to_scm(tv);
}

SCM SchemeSmob::ss_new_itv (SCM slower, SCM supper, SCM sconfidence)
{
	double lower = scm_to_double(slower);
	double upper = scm_to_double(supper);
	double confidence = scm_to_double(sconfidence);

	ProtoAtomPtr tv = new IndefiniteTruthValue(lower, upper, confidence);
	return protom_to_scm(tv);
}

SCM SchemeSmob::ss_new_ptv (SCM smean, SCM sconfidence, SCM scount)
{
	double mean = scm_to_double(smean);
	double confidence = scm_to_double(sconfidence);
	double count = scm_to_double(scount);

	ProtoAtomPtr tv = new ProbabilisticTruthValue(mean, confidence, count);
	return protom_to_scm(tv);
}

SCM SchemeSmob::ss_new_ftv (SCM smean, SCM sconfidence)
{
	double mean = scm_to_double(smean);
	double confidence = scm_to_double(sconfidence);

	float cnt = FuzzyTruthValue::confidenceToCount(confidence);
	ProtoAtomPtr tv = new FuzzyTruthValue(mean, cnt);
	return protom_to_scm(tv);
}

SCM SchemeSmob::ss_new_etv (SCM sposcount, SCM stotalcount)
{
	double pos_count = scm_to_double(sposcount);
	double total_count = scm_to_double(stotalcount);

	ProtoAtomPtr tv = new EvidenceCountTruthValue(pos_count, total_count);
	return protom_to_scm(tv);
}

/* ============================================================== */
/**
 * Return true if the scm is a truth value
 */
SCM SchemeSmob::ss_tv_p (SCM s)
{
	ProtoAtomPtr pa(scm_to_protom(s));
	if (nullptr == pa) return SCM_BOOL_F;

	if (classserver().isA(pa->getType(), TRUTH_VALUE))
		return SCM_BOOL_T;

	scm_remember_upto_here_1(s);
	return SCM_BOOL_F;
}

/**
 * Return true if the scm is a truth value
 */
inline SCM SchemeSmob::tv_p (SCM s, Type wanted)
{
	ProtoAtomPtr pa(scm_to_protom(s));
	if (nullptr == pa) return SCM_BOOL_F;

	if (wanted == pa->getType()) return SCM_BOOL_T;
	scm_remember_upto_here_1(s);
	return SCM_BOOL_F;
}

SCM SchemeSmob::ss_stv_p (SCM s)
{
	return tv_p(s, SIMPLE_TRUTH_VALUE);
}

SCM SchemeSmob::ss_ctv_p (SCM s)
{
	return tv_p(s, COUNT_TRUTH_VALUE);
}

SCM SchemeSmob::ss_itv_p (SCM s)
{
	return tv_p(s, INDEFINITE_TRUTH_VALUE);
}

SCM SchemeSmob::ss_ptv_p (SCM s)
{
	return tv_p(s, PROBABILISTIC_TRUTH_VALUE);
}

SCM SchemeSmob::ss_ftv_p (SCM s)
{
	return tv_p(s, FUZZY_TRUTH_VALUE);
}

SCM SchemeSmob::ss_etv_p (SCM s)
{
	return tv_p(s, EVIDENCE_COUNT_TRUTH_VALUE);
}

/* ============================================================== */

TruthValuePtr SchemeSmob::verify_tv(SCM stv, const char *subrname, int pos)
{

	ProtoAtomPtr pa(scm_to_protom(stv));
	TruthValuePtr tv(TruthValueCast(pa));

	if (nullptr == tv)
		scm_wrong_type_arg_msg(subrname, pos, stv, "opencog truth value");

	return tv;
}

/**
 * Return association list holding contents of a truth value
 */
SCM SchemeSmob::ss_tv_get_value (SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv->alist");
	Type tvt = tv->getType();

	if (SIMPLE_TRUTH_VALUE == tvt)
	{
		SCM mean = scm_from_double(tv->getMean());
		SCM conf = scm_from_double(tv->getConfidence());
		SCM count = scm_from_double(tv->getCount());
		SCM smean = scm_from_utf8_symbol("mean");
		SCM sconf = scm_from_utf8_symbol("confidence");
		SCM scount = scm_from_utf8_symbol("count");

		SCM rc = SCM_EOL;
		rc = scm_acons(sconf, conf, rc);
		rc = scm_acons(smean, mean, rc);
		rc = scm_acons(scount, count, rc);
		scm_remember_upto_here_1(s);
		return rc;
	}

	if (COUNT_TRUTH_VALUE == tvt)
	{
		SCM mean = scm_from_double(tv->getMean());
		SCM conf = scm_from_double(tv->getConfidence());
		SCM cont = scm_from_double(tv->getCount());
		SCM smean = scm_from_utf8_symbol("mean");
		SCM sconf = scm_from_utf8_symbol("confidence");
		SCM scont = scm_from_utf8_symbol("count");

		SCM rc = SCM_EOL;
		rc = scm_acons(scont, cont, rc),
		rc = scm_acons(sconf, conf, rc);
		rc = scm_acons(smean, mean, rc);
		scm_remember_upto_here_1(s);
		return rc;
	}

	if (INDEFINITE_TRUTH_VALUE == tvt)
	{
		IndefiniteTruthValuePtr itv = static_cast<IndefiniteTruthValuePtr>(tv);
		SCM lower = scm_from_double(itv->getL());
		SCM upper = scm_from_double(itv->getU());
		SCM conf_level = scm_from_double(itv->getConfidenceLevel());
		SCM slower = scm_from_utf8_symbol("lower");
		SCM supper = scm_from_utf8_symbol("upper");
		SCM sconf_level = scm_from_utf8_symbol("confidence-level");
	
		SCM rc = SCM_EOL;
		rc = scm_acons(sconf_level, conf_level, rc);
		rc = scm_acons(supper, upper, rc),
		rc = scm_acons(slower, lower, rc);
		scm_remember_upto_here_1(s);
		return rc;
	}

	if (PROBABILISTIC_TRUTH_VALUE == tvt)
	{
		SCM mean = scm_from_double(tv->getMean());
		SCM conf = scm_from_double(tv->getConfidence());
		SCM cont = scm_from_double(tv->getCount());
		SCM smean = scm_from_utf8_symbol("mean");
		SCM sconf = scm_from_utf8_symbol("confidence");
		SCM scont = scm_from_utf8_symbol("count");

		SCM rc = SCM_EOL;
		rc = scm_acons(scont, cont, rc),
		rc = scm_acons(sconf, conf, rc);
		rc = scm_acons(smean, mean, rc);
		scm_remember_upto_here_1(s);
		return rc;
	}

	if (FUZZY_TRUTH_VALUE == tvt)
	{
		SCM mean = scm_from_double(tv->getMean());
		SCM conf = scm_from_double(tv->getConfidence());
		SCM count = scm_from_double(tv->getCount());
		SCM smean = scm_from_utf8_symbol("mean");
		SCM sconf = scm_from_utf8_symbol("confidence");
		SCM scount = scm_from_utf8_symbol("count");

		SCM rc = SCM_EOL;
		rc = scm_acons(sconf, conf, rc);
		rc = scm_acons(smean, mean, rc);
		rc = scm_acons(scount, count, rc);
		scm_remember_upto_here_1(s);
		return rc;
	}

	if	(EVIDENCE_COUNT_TRUTH_VALUE == tvt)
	{
		EvidenceCountTruthValuePtr etv = static_cast<EvidenceCountTruthValuePtr>(tv);
		SCM poscount = scm_from_double(etv->getPositiveCount());
		SCM mean = scm_from_double(etv->getMean());
		SCM conf = scm_from_double(etv->getConfidence());
		SCM count = scm_from_double(etv->getCount());
		SCM sposcount = scm_from_utf8_symbol("positive-count");
		SCM smean = scm_from_utf8_symbol("mean");
		SCM sconf = scm_from_utf8_symbol("confidence");
		SCM scount = scm_from_utf8_symbol("count");

		SCM rc = SCM_EOL;
		rc = scm_acons(sposcount, poscount, rc);
		rc = scm_acons(smean, mean, rc);
		rc = scm_acons(sconf, conf, rc);
		rc = scm_acons(scount, count, rc);
		scm_remember_upto_here_1(s);
		return rc;
	}

	scm_remember_upto_here_1(s);
	return SCM_EOL;
}

/**
 * Return the truth value mean
 */
SCM SchemeSmob::ss_tv_get_mean(SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv-mean");
	return scm_from_double(tv->getMean());
}

/**
 * Return the truth value confidence
 */
SCM SchemeSmob::ss_tv_get_confidence(SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv-confidence");
	return scm_from_double(tv->getConfidence());
}

/**
 * Return the truth value count
 */
SCM SchemeSmob::ss_tv_get_count(SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv-count");
	return scm_from_double(tv->getCount());
}

/* ===================== END OF FILE ============================ */
