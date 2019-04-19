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
#include <memory>
#include <libguile.h>

#include <opencog/atoms/truthvalue/FuzzyTruthValue.h>
#include <opencog/atoms/truthvalue/ProbabilisticTruthValue.h>
#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/IndefiniteTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/atoms/truthvalue/EvidenceCountTruthValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */

#ifdef USE_KEYWORD_LIST_NOT_USED
/**
 * Search for a truth value (demarked by #:tv) in a list of key-value
 * pairs.  Return the truth value if found, else return null.
 * Throw errors if the list is not strictly just key-value pairs
 *
 * XXX This code is not currently used, since it seems pointless
 * to have key-value pairs for this function. After all, an atom
 * can only have one truth value ever -- if we find a truth value, we
 * use it. We don't really need a key to tell us that its a truth value.
 * So punt, and get truth values implicitly. Meanwhile, this code is
 * stubbed out, for a rainy day, in case we need to resurrect key-value
 * pairs in the future.
 */
static TruthValuePtr get_tv_from_kvp(SCM kvp, const char * subrname, int pos)
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
			return scm_to_tv(sval);
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
				case COG_PROTOM: {
					ValuePtr pa(scm_to_protom(sval));
					TruthValuePtr tv(TruthValueCast(pa));
					if (tv) return tv;
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

std::string SchemeSmob::tv_to_string(const TruthValuePtr& tv)
{
#define BUFLEN 120
	char buff[BUFLEN];
	Type tvt = tv->get_type();

	// Pretend they're floats, not doubles, so print with 8 digits
	std::string ret = "";
	if (SIMPLE_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(stv %.8g ", tv->get_mean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->get_confidence());
		ret += buff;
		return ret;
	}

	if (COUNT_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(ctv %.8g ", tv->get_mean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g ", tv->get_confidence());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->get_count());
		ret += buff;
		return ret;
	}

	if (INDEFINITE_TRUTH_VALUE == tvt)
	{
		const IndefiniteTruthValuePtr itv = IndefiniteTVCast(tv);
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
		snprintf(buff, BUFLEN, "(ptv %.8g ", tv->get_mean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g ", tv->get_confidence());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->get_count());
		ret += buff;
		return ret;
	}

	if (FUZZY_TRUTH_VALUE == tvt)
	{
		snprintf(buff, BUFLEN, "(ftv %.8g ", tv->get_mean());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", tv->get_confidence());
		ret += buff;
		return ret;
	}

	if (EVIDENCE_COUNT_TRUTH_VALUE == tvt)
	{
		const EvidenceCountTruthValuePtr etv = std::dynamic_pointer_cast<const EvidenceCountTruthValue>(tv);
		snprintf(buff, BUFLEN, "(etv %.8g ", etv->getPositiveCount());
		ret += buff;
		snprintf(buff, BUFLEN, "%.8g)", etv->get_count());
		ret += buff;
		return ret;
	}
	return ret;
}

/* ============================================================== */

TruthValuePtr SchemeSmob::scm_to_tv(SCM stv)
{
	ValuePtr pa(scm_to_protom(stv));
	TruthValuePtr tv(TruthValueCast(pa));

	return tv;
}

TruthValuePtr SchemeSmob::verify_tv(SCM stv, const char *subrname, int pos)
{
	ValuePtr pa(scm_to_protom(stv));
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
	Type tvt = tv->get_type();

	if (SIMPLE_TRUTH_VALUE == tvt)
	{
		SCM mean = scm_from_double(tv->get_mean());
		SCM conf = scm_from_double(tv->get_confidence());
		SCM count = scm_from_double(tv->get_count());
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
		SCM mean = scm_from_double(tv->get_mean());
		SCM conf = scm_from_double(tv->get_confidence());
		SCM cont = scm_from_double(tv->get_count());
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
		IndefiniteTruthValuePtr itv = IndefiniteTVCast(tv);
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
		SCM mean = scm_from_double(tv->get_mean());
		SCM conf = scm_from_double(tv->get_confidence());
		SCM cont = scm_from_double(tv->get_count());
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
		SCM mean = scm_from_double(tv->get_mean());
		SCM conf = scm_from_double(tv->get_confidence());
		SCM count = scm_from_double(tv->get_count());
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
		EvidenceCountTruthValuePtr etv = std::dynamic_pointer_cast<const EvidenceCountTruthValue>(tv);
		SCM poscount = scm_from_double(etv->getPositiveCount());
		SCM mean = scm_from_double(etv->get_mean());
		SCM conf = scm_from_double(etv->get_confidence());
		SCM count = scm_from_double(etv->get_count());
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
 * Return the truth value mean.
 * This is meant to be the fastest-possible of getting the mean.
 */
SCM SchemeSmob::ss_tv_get_mean(SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv-mean");
	return scm_from_double(tv->get_mean());
}

/**
 * Return the truth value confidence
 * This is meant to be the fastest-possible of getting the confidence.
 */
SCM SchemeSmob::ss_tv_get_confidence(SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv-confidence");
	return scm_from_double(tv->get_confidence());
}

/**
 * Return the truth value count
 * This is meant to be the fastest-possible of getting the count.
 */
SCM SchemeSmob::ss_tv_get_count(SCM s)
{
	TruthValuePtr tv = verify_tv(s, "cog-tv-count");
	return scm_from_double(tv->get_count());
}

#define tv_to_scm(TV) protom_to_scm(ValueCast(TV))

SCM SchemeSmob::ss_tv_merge (SCM sta, SCM stb)
{
	TruthValuePtr tva = verify_tv(sta, "cog-tv-merge", 1);
	TruthValuePtr tvb = verify_tv(stb, "cog-tv-merge", 2);

	return tv_to_scm(tva->merge(tvb));
}

SCM SchemeSmob::ss_tv_merge_hi_conf (SCM sta, SCM stb)
{
	TruthValuePtr tva = verify_tv(sta, "cog-tv-merge-hi-conf", 1);
	TruthValuePtr tvb = verify_tv(stb, "cog-tv-merge-hi-conf", 2);

	return tv_to_scm(tva->merge(tvb,
		MergeCtrl(MergeCtrl::TVFormula::HIGHER_CONFIDENCE)));
}

/* ===================== END OF FILE ============================ */
