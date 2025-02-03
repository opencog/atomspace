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

#include <opencog/atoms/truthvalue/CountTruthValue.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */
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

TruthValuePtr SchemeSmob::verify_tv(SCM stv, const char *subrname, int pos)
{
	ValuePtr pa(scm_to_protom(stv));
	TruthValuePtr tv(TruthValueCast(pa));

	if (nullptr == tv)
		scm_wrong_type_arg_msg(subrname, pos, stv, "opencog truth value");

	return tv;
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
