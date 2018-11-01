from libcpp.set cimport set
from opencog.atomspace cimport cHandle, cAtomSpace


cdef extern from "opencog/rule-engine/backwardchainer/Fitness.h" namespace "opencog::BITNodeFitness":
    cdef cppclass BitNodeFitnessType:
        pass


cdef extern from "opencog/rule-engine/backwardchainer/Fitness.h" namespace "opencog::AndBITNodeFitness":
    cdef cppclass AndBitFitnessType:
        pass

cdef extern from "opencog/rule-engine/backwardchainer/Fitness.h" namespace "opencog::AndBITFitness::FitnessType":
    cdef AndBitFitnessType Uniform
    cdef AndBitFitnessType Trace 


cdef extern from "opencog/rule-engine/backwardchainer/Fitness.h" namespace "opencog::BITNodeFitness::FitnessType":
    cdef BitNodeFitnessType MaximizeConfidence


cdef extern from "opencog/rule-engine/backwardchainer/Fitness.h" namespace "opencog":
    cdef cppclass ContentHash:
        pass

    cdef cppclass BITNodeFitness:
        BITNodeFitness(BitNodeFitnessType ft)

    cdef cppclass AndBITFitness:
        AndBITFitness(AndBitFitnessType ft,
                      const set[ContentHash]& tr);


cdef extern from "opencog/rule-engine/backwardchainer/BackwardChainer.h" namespace "opencog":
    cdef cppclass cBackwardChainer "opencog::BackwardChainer":
        cBackwardChainer(cAtomSpace& _as, 
                        const cHandle& rbs,
                        const cHandle& target,
                        const cHandle& vardecl,
                        cAtomSpace* trace_as,
                        cAtomSpace* control_as,
                        const cHandle& focus_set,
                        const BITNodeFitness& bitnode_fitness,
                        const AndBITFitness& andbit_fitness) except +

        cBackwardChainer(cAtomSpace& _as, 
                        const cHandle& rbs,
                        const cHandle& target,
                        const cHandle& vardecl,
                        cAtomSpace* trace_as,
                        cAtomSpace* control_as,
                        const cHandle& focus_set) except +

        void do_chain()
        cHandle get_results() const

