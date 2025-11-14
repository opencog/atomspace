
from opencog.atomspace import AtomSpace, types
from opencog.utilities import get_default_atomspace
import opencog.scheme as scheme
from opencog.scheme import scheme_eval
from opencog.type_constructors import *

executed = False

def add_link(atom1, atom2):
    global executed
    link = ListLink(atom1, atom2)
    executed = True
    return link

# Module that defines cog-execute!
atomspace = get_default_atomspace()
scheme_eval(atomspace, '(use-modules (opencog exec))')

execute_code = \
    '''
    (cog-execute!
        (ExecutionOutputLink
            (GroundedSchemaNode \"py: add_link\")
            (ListLink
                (ConceptNode \"one\")
                (ConceptNode \"two\")
            )
        )
    )
    '''
scheme_eval(atomspace, execute_code)

print ("execute: cog-execute")
if (executed):
    print ("add_link - executed successfully")
else:
    print ("add_link - did NOT execute")

executed = False
ExecutionOutputLink(
    GroundedSchemaNode("py: add_link"),
    ListLink(
        ConceptNode("one"),
        ConceptNode("two")
    )
).execute()
print ("execute: execute_atom")
if (executed):
    print ("add_link - executed successfully")
else:
    print ("add_link - did NOT execute")
