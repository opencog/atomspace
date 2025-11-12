"""
Shared utilities for multi-threading tests.

This module provides base classes, decorators, and utilities for testing
thread safety in the OpenCog Python bindings.
"""

import unittest
import threading
import time
import sys
import os
import gc
from typing import Dict, List, Any, Callable, Optional
from functools import wraps

# Optional: psutil for memory monitoring
try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    HAS_PSUTIL = False
    print("WARNING: psutil not available, memory monitoring disabled")


class MemoryMonitor:
    """Context manager for monitoring memory usage during tests."""

    def __init__(self, name: str, tolerance_mb: float = 10.0):
        """
        Initialize memory monitor.

        Args:
            name: Test name for logging
            tolerance_mb: Acceptable memory increase in MB
        """
        self.name = name
        self.tolerance_mb = tolerance_mb
        self.process = psutil.Process() if HAS_PSUTIL else None
        self.start_rss = 0
        self.end_rss = 0
        self.enabled = HAS_PSUTIL

    def __enter__(self):
        """Start monitoring."""
        if not self.enabled:
            return self
        gc.collect()  # Clean up first
        self.start_rss = self.process.memory_info().rss / 1024 / 1024  # MB
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Check memory after test."""
        if not self.enabled:
            return False
        gc.collect()
        self.end_rss = self.process.memory_info().rss / 1024 / 1024  # MB
        growth = self.end_rss - self.start_rss

        if growth > self.tolerance_mb:
            print(f"\nWARNING [{self.name}]: Memory grew by {growth:.2f} MB "
                  f"(tolerance: {self.tolerance_mb:.2f} MB)")
        return False

    def get_growth_mb(self) -> float:
        """Get memory growth in MB."""
        if not self.enabled:
            return 0.0
        return self.end_rss - self.start_rss


class ThreadSafetyValidator:
    """Validator for detecting race conditions and thread safety issues."""

    def __init__(self):
        self.errors: List[str] = []
        self.lock = threading.Lock()

    def record_error(self, thread_id: int, error: str):
        """Record a thread safety error."""
        with self.lock:
            self.errors.append(f"Thread {thread_id}: {error}")

    def assert_no_errors(self, test_case: unittest.TestCase):
        """Assert that no errors were recorded."""
        if self.errors:
            error_msg = "\n".join(self.errors)
            test_case.fail(f"Thread safety violations detected:\n{error_msg}")

    def get_error_count(self) -> int:
        """Get number of errors."""
        return len(self.errors)


class ThreadTestCase(unittest.TestCase):
    """Base class for threading tests with common utilities."""

    def run_threads(self,
                   worker: Callable,
                   num_threads: int,
                   use_barrier: bool = False,
                   timeout: float = 30.0,
                   thread_args: Optional[List] = None) -> Dict[int, Any]:
        """
        Run multiple threads and collect results.

        Args:
            worker: Worker function taking (thread_id, *args)
            num_threads: Number of threads to create
            use_barrier: Use barrier for synchronized start
            timeout: Timeout in seconds per thread
            thread_args: Optional list of args per thread

        Returns:
            Dict mapping thread_id to result/exception
        """
        results = {}
        exceptions = {}
        barrier = threading.Barrier(num_threads) if use_barrier else None

        def thread_wrapper(thread_id: int, *args):
            """Wrapper that captures results and exceptions."""
            try:
                if barrier:
                    barrier.wait()
                result = worker(thread_id, *args)
                results[thread_id] = result
            except Exception as e:
                exceptions[thread_id] = e
                results[thread_id] = None

        # Create and start threads
        threads = []
        for i in range(num_threads):
            args = thread_args[i] if thread_args else ()
            thread = threading.Thread(
                target=thread_wrapper,
                args=(i,) + args,
                name=f"TestWorker-{i}"
            )
            threads.append(thread)
            thread.start()

        # Wait for completion
        for i, thread in enumerate(threads):
            thread.join(timeout=timeout)
            self.assertFalse(
                thread.is_alive(),
                f"Thread {i} ({thread.name}) did not complete within {timeout}s"
            )

        # Check for exceptions
        if exceptions:
            error_summary = "\n".join(
                f"  Thread {tid}: {exc}"
                for tid, exc in exceptions.items()
            )
            self.fail(
                f"{len(exceptions)}/{num_threads} threads failed with exceptions:\n"
                f"{error_summary}"
            )

        return results

    def measure_concurrent_performance(self,
                                     worker: Callable,
                                     num_threads: int,
                                     iterations: int = 1) -> Dict[str, float]:
        """
        Measure performance of concurrent execution.

        Returns dict with:
            - total_time: Total wall clock time
            - avg_time_per_thread: Average time per thread
            - operations_per_sec: Total operations per second
        """
        start_time = time.time()

        def timed_worker(thread_id):
            for _ in range(iterations):
                worker(thread_id)

        self.run_threads(timed_worker, num_threads, use_barrier=True)

        total_time = time.time() - start_time
        total_ops = num_threads * iterations

        return {
            'total_time': total_time,
            'avg_time_per_thread': total_time,  # Wall clock, not CPU time
            'operations_per_sec': total_ops / total_time if total_time > 0 else 0
        }

    def assert_linear_scaling(self,
                            results: Dict[str, float],
                            baseline_time: float,
                            num_threads: int,
                            tolerance: float = 2.0):
        """
        Assert that execution time scales roughly linearly with threads.

        For I/O-bound operations, we expect near-linear scaling.
        tolerance=2.0 means we accept up to 2x the ideal time.
        """
        actual_time = results['total_time']
        ideal_time = baseline_time  # Should complete in similar time with more threads
        max_acceptable_time = ideal_time * tolerance

        self.assertLess(
            actual_time, max_acceptable_time,
            f"Execution time {actual_time:.2f}s exceeds acceptable "
            f"{max_acceptable_time:.2f}s (baseline: {baseline_time:.2f}s, "
            f"threads: {num_threads})"
        )


def requires_python_version(min_version: str):
    """Decorator to skip test if Python version too old."""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            min_tuple = tuple(map(int, min_version.split('.')))
            current_tuple = sys.version_info[:len(min_tuple)]
            if current_tuple < min_tuple:
                raise unittest.SkipTest(
                    f"Requires Python {min_version}+, current: "
                    f"{sys.version_info.major}.{sys.version_info.minor}"
                )
            return func(*args, **kwargs)
        return wrapper
    return decorator


def timeout(seconds: float):
    """Decorator to add timeout to test."""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            result = [None]
            exception = [None]

            def target():
                try:
                    result[0] = func(*args, **kwargs)
                except Exception as e:
                    exception[0] = e

            thread = threading.Thread(target=target)
            thread.daemon = True
            thread.start()
            thread.join(timeout=seconds)

            if thread.is_alive():
                raise TimeoutError(f"Test exceeded timeout of {seconds}s")

            if exception[0]:
                raise exception[0]

            return result[0]
        return wrapper
    return decorator


def check_memory_leaks(tolerance_mb: float = 10.0):
    """Decorator to check for memory leaks."""
    def decorator(func):
        @wraps(func)
        def wrapper(self, *args, **kwargs):
            if not HAS_PSUTIL:
                # Skip memory checking if psutil not available
                return func(self, *args, **kwargs)

            with MemoryMonitor(func.__name__, tolerance_mb) as monitor:
                result = func(self, *args, **kwargs)

            growth = monitor.get_growth_mb()
            if growth > tolerance_mb:
                self.fail(
                    f"Memory leak detected: {growth:.2f} MB growth "
                    f"(tolerance: {tolerance_mb:.2f} MB)"
                )
            return result
        return wrapper
    return decorator


class TestHelperModule:
    """
    Helper class providing test functions for multi-threading tests.

    These functions are designed to be called from GroundedSchemas in
    multi-threaded contexts.
    """

    @staticmethod
    def simple_function():
        """Simple function returning a ConceptNode."""
        from opencog.type_constructors import ConceptNode
        return ConceptNode("success")

    @staticmethod
    def function_with_sleep(sleep_ms: int = 100):
        """Function with simulated I/O delay."""
        from opencog.type_constructors import ConceptNode
        time.sleep(sleep_ms / 1000.0)
        return ConceptNode(f"slept_{sleep_ms}ms")

    @staticmethod
    def function_with_args(arg1, arg2):
        """Function that processes arguments."""
        from opencog.type_constructors import ConceptNode
        return ConceptNode(f"result_{arg1.name}_{arg2.name}")

    @staticmethod
    def cpu_intensive(iterations: int = 1000):
        """CPU-intensive function for GIL contention testing."""
        from opencog.type_constructors import NumberNode
        result = 0
        for i in range(iterations):
            result += i * i
        return NumberNode(str(result))

    @staticmethod
    def function_that_raises():
        """Function that always raises an exception."""
        raise RuntimeError("Deliberate test exception")

    @staticmethod
    def function_returning_none():
        """Function that returns None (should cause error)."""
        return None


# Register helper functions in __main__ for easy access
import __main__
__main__.simple_function = TestHelperModule.simple_function
__main__.function_with_sleep = TestHelperModule.function_with_sleep
__main__.function_with_args = TestHelperModule.function_with_args
__main__.cpu_intensive = TestHelperModule.cpu_intensive
__main__.function_that_raises = TestHelperModule.function_that_raises
__main__.function_returning_none = TestHelperModule.function_returning_none
