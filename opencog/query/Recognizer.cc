/*
 * Recognizer experiment
 */

#ifndef _OPENCOG_RECOGNIZER_H
#define _OPENCOG_RECOGNIZER_H

#include <opencog/query/DefaultPatternMatchCB.h>
#include <opencog/atoms/bind/PatternLink.h>

#include "BindLinkAPI.h"

namespace opencog {

class Recognizer :
   public virtual DefaultPatternMatchCB
{
	protected:
		const Pattern* _pattern;

		Handle _root;
		Handle _starter_term;
		size_t _cnt;
		bool do_search(PatternMatchEngine*, const Handle&);

	public:
		Recognizer(AtomSpace* as) :
			DefaultPatternMatchCB(as) {}

		virtual bool initiate_search(PatternMatchEngine*);
		virtual void set_pattern(const Variables& vars,
										 const Pattern& pat)
		{
			_pattern = &pat;
			DefaultPatternMatchCB::set_pattern(vars, pat);
		}

		virtual bool grounding(const std::map<Handle, Handle> &var_soln,
		                       const std::map<Handle, Handle> &term_soln);
};

} // namespace opencog

#endif // _OPENCOG_RECOGNIZER_H

using namespace opencog;

// Uncomment below to enable debug print
// #define DEBUG
#ifdef DEBUG
#define dbgprt(f, varargs...) printf(f, ##varargs)
#else
#define dbgprt(f, varargs...)
#endif

/* ======================================================== */

bool Recognizer::do_search(PatternMatchEngine* pme, const Handle& top)
{
	LinkPtr ltop(LinkCast(top));
	if (ltop)
	{
		for (const Handle& h : ltop->getOutgoingSet())
		{
			_starter_term = top;
			bool found = do_search(pme, h);
			if (found) return true;
		}
		return false;
	}

	IncomingSet iset = get_incoming_set(top);
	size_t sz = iset.size();
	for (size_t i = 0; i < sz; i++)
	{
		Handle h(iset[i]);
		dbgprt("rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr\n");
		dbgprt("Loop candidate (%lu - %s):\n%s\n", _cnt++,
		       top->toShortString().c_str(),
		       h->toShortString().c_str());
		bool found = pme->explore_neighborhood(_root, _starter_term, h);

		// Terminate search if satisfied.
		if (found) return true;
   }

	return false;
}

bool Recognizer::initiate_search(PatternMatchEngine* pme)
{
	const HandleSeq& clauses = _pattern->cnf_clauses;

	_cnt = 0;
	for (const Handle& h: clauses)
	{
		_root = h;
		bool found = do_search(pme, h);
		if (found) return true;
	}
	return false;
}

bool Recognizer::grounding(const std::map<Handle, Handle> &var_soln,
                           const std::map<Handle, Handle> &term_soln)
{
printf("duuuude ola!!\n");

	// Look for more groundings.
	return false;
}

Handle opencog::recognize(AtomSpace* as, const Handle& hlink)
{
	PatternLinkPtr bl(PatternLinkCast(hlink));
	if (NULL == bl)
		bl = createPatternLink(hlink);

	Recognizer reco(as);
	bl->satisfy(reco);

	return Handle(createLink(LIST_LINK, HandleSeq()));
}
