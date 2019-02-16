/*
 * SchemeSmobAV.c
 *
 * Scheme small objects (SMOBS) for attention values.
 *
 * Copyright (c) 2008,2009 Linas Vepstas <linas@linas.org>
 */

#include <cstddef>
#include <libguile.h>

#include <opencog/atoms/truthvalue/AttentionValue.h>
#include <opencog/guile/SchemeSmob.h>

using namespace opencog;

/* ============================================================== */

SCM SchemeSmob::av_to_scm (const AttentionValuePtr& av)
{
	return protom_to_scm(ValueCast(av));
}

/* ============================================================== */

AttentionValuePtr SchemeSmob::verify_av(SCM sav, const char *subrname, int pos)
{
	ValuePtr pa(scm_to_protom(sav));
	AttentionValuePtr av(AttentionValueCast(pa));

	if (nullptr == av)
		scm_wrong_type_arg_msg(subrname, pos, sav, "opencog attention value");

	return av;
}

/* ===================== END OF FILE ============================ */
