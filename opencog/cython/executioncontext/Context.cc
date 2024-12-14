#include "Context.h"

namespace opencog {

   thread_local std::deque<AtomSpacePtr> current = std::deque<AtomSpacePtr>();

   AtomSpacePtr get_context_atomspace() {
       if (current.empty())
           return nullptr;
       return current.back();
   }

   void push_context_atomspace(AtomSpacePtr asp) {
       current.push_back(asp);
   }

   void push_context_atomspace(ValuePtr vasp) {
       AtomSpacePtr asp = AtomSpaceCast(vasp);
       current.push_back(asp);
   }

   AtomSpacePtr pop_context_atomspace() {
       AtomSpacePtr result = get_context_atomspace();
       current.pop_back();
       return result;
   }

   void clear_context(){
       current.clear();
   }

} // namespace
