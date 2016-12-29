
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/NumberNode.h>
#include <opencog/atoms/reduct/ArithmeticLink.h>

#include "Instantiator.h"

using namespace opencog;

static inline double get_double(AtomSpace *as, Handle h)
{
   NumberNodePtr nnn(NumberNodeCast(h));
   if (nnn == NULL)
      throw RuntimeException(TRACE_INFO,
           "Expecting a NumberNode, got %s",
           classserver().getTypeName(h->getType()).c_str());

   return nnn->getValue();
}

// Work around another python bug.  Why is python so fucking buggy?
// This doesn't belong in this directory, except that I cannot get
// things to compile in any other way, due to a circular library
// dependency in the python bindings. Arghh...
//
Handle ArithmeticLink::execute(AtomSpace* as) const
{
   Instantiator inst(as);
   double sum = knild;
   for (Handle h: _outgoing)
   {
      h = inst.execute(h);
      sum = konsd(sum, get_double(as, h));
   }

   if (as) return as->addAtom(createNumberNode(sum));
   return Handle(createNumberNode(sum));
}


