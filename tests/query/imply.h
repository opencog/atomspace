
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/core/FindUtils.h>
#include <opencog/atoms/pattern/QueryLink.h>
#include <opencog/atoms/value/QueueValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/Implicator.h>

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
	Handle hvars(createLink(std::move(vars), VARIABLE_LIST));

	HandleSeq oset = {hvars, hclauses, himplicand};

	QueryLinkPtr bl(createQueryLink(std::move(oset)));

	// Now perform the search.
	QueueValuePtr qvp(createQueueValue());
	ContainerValuePtr cvp(qvp);
	qvp->close();
	Implicator impl(as, cvp);
	impl.satisfy(bl);

	// The result_set contains a list of the grounded expressions.
	// Turn it into a true list, and return it.
	OC_ASSERT(qvp->is_closed(), "Unexpected queue state!");
	HandleSeq hlist(qvp->to_handle_seq());
	Handle gl = as->add_link(LIST_LINK, std::move(hlist));
	return gl;
}
static inline Handle imply(AtomSpacePtr as, Handle hclauses, Handle himplicand)
{
	return imply(as.get(), hclauses, himplicand);
}
