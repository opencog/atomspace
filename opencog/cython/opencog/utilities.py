"""
Backwards compatibility shim for the removed opencog.utilities module.

This module has been deprecated. All functionality has been moved to
opencog.atomspace. Please update your code to import from opencog.atomspace
directly.

Example:
    Old (deprecated):
        from opencog.utilities import AtomSpace

    New (recommended):
        from opencog.atomspace import AtomSpace
"""

import warnings

# Issue deprecation warning
warnings.warn(
    "The 'opencog.utilities' module is deprecated and will be removed in a "
    "future version. Please use 'from opencog.atomspace import ...' directly.",
    DeprecationWarning,
    stacklevel=2
)

# Re-export everything from opencog.atomspace
from opencog.atomspace import *

# Re-export everything from opencog.type_ctors
from opencog.type_ctors import *


# Backwards-compatibility wrappers (deprecated)
def set_default_atomspace(atomspace):
    """
    Deprecated: Use set_thread_atomspace instead.

    Set the default atomspace for the current thread.
    """
    warnings.warn("set_default_atomspace is deprecated, use set_thread_atomspace instead",
                  DeprecationWarning, stacklevel=2)
    set_thread_atomspace(atomspace)


def get_default_atomspace():
    """
    Deprecated: Use get_thread_atomspace instead.

    Get the default atomspace for the current thread.
    """
    warnings.warn("get_default_atomspace is deprecated, use get_thread_atomspace instead",
                  DeprecationWarning, stacklevel=2)
    return get_thread_atomspace()


# Ensure __all__ is propagated if it exists
try:
    from opencog.atomspace import __all__ as _atomspace_all
    from opencog.type_ctors import __all__ as _type_ctors_all
    __all__ = list(set(_atomspace_all + _type_ctors_all))
except ImportError:
    pass
