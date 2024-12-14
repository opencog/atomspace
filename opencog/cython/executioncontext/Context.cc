#include "Context.h"

namespace opencog {

   thread_local std::deque<AtomSpacePtr> current = std::deque<AtomSpacePtr>();

   AtomSpace* get_context_atomspace(){
       if (current.empty())
           return nullptr;
       return current.back().get();
   }
   void push_context_atomspace(AtomSpace * atomspace){
       AtomSpacePtr asp(atomspace);
       current.push_back(asp);
   }

   AtomSpace * pop_context_atomspace(){
       AtomSpace * result = get_context_atomspace();
       current.pop_back();
       return result;
   }

   void clear_context(){
       current.clear();
   }

} // namespace
