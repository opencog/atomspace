"""
Helper module for testing GroundedSchema with threading.

This module provides simple functions that can be called via GroundedSchema
to test module resolution in multi-threaded contexts.
"""

from opencog.type_constructors import ConceptNode, NumberNode


def simple_function():
    """Simple function that returns a ConceptNode."""
    return ConceptNode("success")


def function_with_args(arg1, arg2):
    """Function that takes arguments and returns a result."""
    return ConceptNode(f"result_{arg1.name}_{arg2.name}")


def numeric_function(num):
    """Function that processes a NumberNode."""
    value = float(num.name)
    result_value = value * 2
    return NumberNode(str(result_value))


def identity_function(atom):
    """Function that returns its input."""
    return atom
