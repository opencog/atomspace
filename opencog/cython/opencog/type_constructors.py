"""
Backwards compatibility shim for the deprecated opencog.type_constructors module.

This module has been deprecated. All functionality has been moved to
opencog.atomspace. Please update your code to import from opencog.atomspace
directly.

Example:
    Old (deprecated):
        from opencog.type_constructors import ConceptNode, ListLink

    New (recommended):
        from opencog.atomspace import ConceptNode, ListLink
"""

import warnings

# Issue deprecation warning on import
warnings.warn(
    "The 'opencog.type_constructors' module is deprecated and will be removed "
    "in a future version. Please use 'from opencog.atomspace import ...' directly.",
    DeprecationWarning,
    stacklevel=2
)

# Re-export everything from atomspace
from opencog.atomspace import *

# Value wrapper functions - these provide convenience constructors
# that shadow the class names. In atomspace, use createBoolValue etc.
from opencog.atomspace import (
    createBoolValue, createFloatValue, createLinkValue,
    createQueueValue, createStringValue, createUnisetValue, createVoidValue
)

def BoolValue(arg):
    return createBoolValue(arg)

def FloatValue(arg):
    return createFloatValue(arg)

def LinkValue(arg):
    return createLinkValue(arg)

def QueueValue(arg=None):
    return createQueueValue(arg)

def StringValue(arg):
    return createStringValue(arg)

def UnisetValue(arg=None):
    return createUnisetValue(arg)

def VoidValue():
    return createVoidValue()

# Ensure __all__ is propagated if it exists
try:
    from opencog.atomspace import __all__ as _atomspace_all
    __all__ = list(_atomspace_all) + [
        'BoolValue', 'FloatValue', 'LinkValue', 'QueueValue',
        'StringValue', 'UnisetValue', 'VoidValue'
    ]
except ImportError:
    pass
