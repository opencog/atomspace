
#include <opencog/atomutils/FindUtils.h>
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

	bl->imply(impl);

	// The result_list contains a list of the grounded expressions.
	// Turn it into a true list, and return it.
	Handle gl = as->add_link(LIST_LINK, impl.get_result_list());
	return gl;
}

/**
 * Pattern Matcher. Just run the matcher against the indicated
 * variables and clauses, using the indicated callback.
 */
static inline void match(PatternMatchCallback& pmcb,
                         const OrderedHandleSet &vars,
                         const HandleSeq &clauses)
{
	PatternLinkPtr slp(createPatternLink(vars, clauses));
	slp->satisfy(pmcb);
}
