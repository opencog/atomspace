#include "Context.h"

namespace opencog {

   thread_local std::deque<AtomSpace*> current = std::deque<AtomSpace* >();

   AtomSpace* get_context_atomspace(){
       if (current.empty())
           return nullptr;
       return current.back();
   }
   void push_context_atomspace(AtomSpace * atomspace){
       current.push_back(atomspace);
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
