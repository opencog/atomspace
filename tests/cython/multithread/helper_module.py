"""
Helper module for threading tests.

This module provides functions that are imported and called from GroundedSchemas
in multi-threaded test scenarios.
"""

import time
from opencog.type_constructors import ConceptNode, NumberNode


def simple_function():
    """Simple function returning a ConceptNode."""
    return ConceptNode("success")


def function_with_args(arg1, arg2):
    """Function that processes two arguments."""
    return ConceptNode(f"result_{arg1.name}_{arg2.name}")


def function_with_sleep(duration_ms=100):
    """Function with simulated I/O delay."""
    time.sleep(duration_ms / 1000.0)
    return ConceptNode(f"slept_{duration_ms}ms")


def cpu_intensive_function(iterations=10000):
    """CPU-intensive function for testing GIL contention."""
    result = 0
    for i in range(iterations):
        result += i * i
    return NumberNode(str(result))


def function_a():
    """Test function A."""
    return ConceptNode("result_a")


def function_b():
    """Test function B."""
    return ConceptNode("result_b")


def function_c():
    """Test function C."""
    return ConceptNode("result_c")


def function_d():
    """Test function D."""
    return ConceptNode("result_d")


def function_e():
    """Test function E."""
    return ConceptNode("result_e")


def function_returning_none():
    """Function that returns None."""
    return None


def function_that_raises():
    """Function that raises an exception."""
    raise RuntimeError("Deliberate test exception from helper_module")


def function_with_type_error():
    """Function that raises TypeError."""
    raise TypeError("Type error from helper_module")


def function_with_value_error():
    """Function that raises ValueError."""
    raise ValueError("Value error from helper_module")


def function_with_division_by_zero():
    """Function that raises ZeroDivisionError."""
    x = 1 / 0
    return NumberNode(str(x))
