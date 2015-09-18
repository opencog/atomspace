/*
 * Recognizer experiment
 */

#include <opencog/atoms/bind/PatternLink.h>
#include <opencog/query/InitiateSearchCB.h>
#include <opencog/query/DefaultPatternMatchCB.h>


#include "BindLinkAPI.h"

using namespace opencog;

class Recognizer :
   public virtual InitiateSearchCB,
   public virtual DefaultPatternMatchCB
{
	public:
		Recognizer(AtomSpace* as) :
			InitiateSearchCB(as),
			DefaultPatternMatchCB(as) {}

		virtual void set_pattern(const Variables& vars,
										 const Pattern& pat)
		{
			InitiateSearchCB::set_pattern(vars, pat);
			DefaultPatternMatchCB::set_pattern(vars, pat);
		}

		virtual bool grounding(const std::map<Handle, Handle> &var_soln,
		                       const std::map<Handle, Handle> &term_soln);
};

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
