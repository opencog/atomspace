#include <opencog/atomspace/AtomSpace.h>
#include <opencog/rule-engine/backwardchainer/BackwardChainer.h>
#include <opencog/util/Config.h>
#include <opencog/rule-engine/forwardchainer/ForwardChainer.h>
#include <opencog/guile/load-file.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/rule-engine/UREConfigReader.h>

using namespace opencog;

void backward_chain(AtomSpace&);

void forward_chain(AtomSpace&);

int main(int argc, char** args)
{
    AtomSpace as;

    //Load core types
    config().set("SCM_PRELOAD", "/usr/local/share/opencog/scm/core_types.scm, "
                 "/usr/local/share/opencog/scm/utilities.scm, "
                 "/usr/local/share/opencog/scm/av-tv.scm");
    load_scm_files_from_config(as);

    std::cout << "Backward chaining to solve the criminal problem:\n";
    backward_chain(as);

    std::cout << "\nForward chaining demo:\n";
    forward_chain(as);

    return 0;
}

/*
 * Demonstrates how to invoke backward chaining and retrieve results back.
 * Here the backward chainer solves the criminal problem see section 9.4
 * of the AIMA book by Stuart J. Russell and Peter Norvig.
 *
 *  @params as The atomspace where backward chaining is made.
 */
void backward_chain(AtomSpace& as)
{
    config().set("SCM_PRELOAD", "examples/c++-api/scm/bc-criminal.scm,"
                 "examples/c++-api/scm/bc-config.scm");
    load_scm_files_from_config(as);

    SchemeEval eval(&as);
    Handle target_var = eval.eval_h("(VariableNode \"$who\")");
    Handle target = eval.eval_h("(InheritanceLink"
                                "   (VariableNode \"$who\")"
                                "   (ConceptNode \"criminal\"))");

    //Create BackwardChainer object.
    Handle hrbase = as.get_node(CONCEPT_NODE, "BC_DEMO_RB");
    BackwardChainer bc(as, hrbase);

    bc.set_target(target);
    //Set maximum number of iteration(backward chaining steps).
    //See http://wiki.opencog.org/w/Unified_Rule_Engine#Overall_Backward_Chaining_Process
    bc.get_config().set_maximum_iterations(1000);
    bc.do_chain();

    VarMultimap results = bc.get_chaining_result();

    std::cout << "Query:\n" << target->toShortString() << std::endl;
    std::cout << "Answer:\n";
    for (const auto& h : results[target_var])
        std::cout << h->toShortString() << std::endl;
}

/*
 * Demonstrates how to invoke backward chaining and retrieve results back.
 *
 *  @params as The atomspace where backward chaining is made.
 */
void forward_chain(AtomSpace& as)
{
    config().set("SCM_PRELOAD", "examples/c++-api/scm/deduction.scm, "
                 "examples/c++-api/scm/fc-simple-assertions.scm, "
                 "examples/c++-api/scm/fc-config.scm");
    load_scm_files_from_config (as);

    SchemeEval eval(&as);

    //Choose source atoms to apply forward chaining on.
    Handle source =
            eval.eval_h(
                    R"((ConceptNode "Socrates"))");

    Handle rbs = as.get_node(CONCEPT_NODE, "FC_DEMO_RB");

    //The final argument to ForwardChainer constructor is used to pass
    //focus set atoms; when the focus set is not empty, the forward chainer
    //is constrained to apply rules only on the focus set atoms and applies
    //on the entire atomspace when focus set is empty.
    ForwardChainer fc(as, rbs,source, HandleSeq { });

    //Start chaining.
    //fc.do_step(); //Single step forward chaining.
    fc.do_chain();

    std::cout << "Forward chaining on source:\n" << source->toShortString() << std::endl;
    std::cout << "FC results:\n";
    for(const auto& h : fc.get_chaining_result())
        std::cout << h->toShortString() << std::endl;
}

