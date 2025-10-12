from opencog.atomspace cimport Atom, AtomSpace

def execute_atom(AtomSpace atomspace, Atom atom):
    return atomspace.execute(atom)
