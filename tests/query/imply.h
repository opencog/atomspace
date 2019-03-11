
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/DefaultImplicator.h>

using namespace opencog;

/**
 * Default evaluator of implication statements.  Does not consider
 * the truth value of any of the matched clauses; instead, looks
 * purely for a structural match.
 */
static inline Handle imply(AtomSpace* as, Handle hclauses, Handle himplicand)
{
	// Extract the variables; they were not specified.
	FindAtoms fv(VARIABLE_NODE);
	fv.search_set(hclauses);

	HandleSeq vars(fv.varset.begin(), fv.varset.end());

	// Stuff the variables into a proper variable list.
	Handle hvars(createLink(vars, VARIABLE_LIST));

	HandleSeq oset = {hvars, hclauses, himplicand};

	BindLinkPtr bl(createBindLink(oset));

	// Now perform the search.
	DefaultImplicator impl(as);
	impl.implicand = himplicand;

	bl->satisfy(impl);

	// The result_set contains a list of the grounded expressions.
	// Turn it into a true list, and return it.
	HandleSeq hlist;
	for (const ValuePtr& v: impl.get_result_set())
		hlist.push_back(HandleCast(v));
	Handle gl = as->add_link(LIST_LINK,hlist);
	return gl;
}

/**
 * Pattern Matcher. Just run the matcher against the indicated
 * variables and clauses, using the indicated callback.
 */
static inline void match(PatternMatchCallback& pmcb,
                         const HandleSet &vars,
                         const HandleSeq &clauses)
{
	PatternLinkPtr slp(createPatternLink(vars, clauses));
	slp->satisfy(pmcb);
}

static inline Handle bindlink(AtomSpace* as,
                          const Handle& hlink, size_t foo=0)
{
	return HandleCast(hlink->execute(as));
}

static inline Handle satisfying_set(AtomSpace* as,
                          const Handle& hlink, size_t foo=0)
{
	return HandleCast(hlink->execute(as));
}
