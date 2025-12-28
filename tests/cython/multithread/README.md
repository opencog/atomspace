# Multi-Threading Tests for OpenCog Python Bindings

This directory contains comprehensive multi-threading tests for the OpenCog Python bindings.

## Test Organization

The tests are organized into 7 phases:

### Phase 1: Baseline Thread Safety Tests (`test_concurrent_eval.py`)
- **Test 1.1**: Concurrent PythonEval Creation/Destruction
- **Test 1.2**: Concurrent Function Calls (Same Function)
- **Test 1.3**: Concurrent Function Calls (Different Functions)

### Phase 2: AtomSpace Isolation Tests (`test_atomspace_isolation.py`)
- **Test 2.1**: Thread-Local AtomSpaces (No Sharing)
- **Test 2.2**: Shared AtomSpace (Read-Heavy)
- **Test 2.3**: Shared AtomSpace (Read-Write Mix)
- **Test 2.4**: AtomSpace Parent-Child Relationships

### Phase 3: Error Handling Tests (`test_error_isolation.py`)
- **Test 3.1**: Exception Isolation
- **Test 3.2**: Module Import Errors
- **Test 3.3**: Invalid Function Calls

**NOTE**: Phase 3 tests are now **ENABLED** after thread-local PythonEval implementation. Exception handling is now fast and concurrent with thread-local instances (tests run in ~1 second vs 55+ seconds with the old singleton).

### Phase 4: Resource Management Tests (`test_resource_management.py`)
- **Test 4.1**: Memory Leak Detection (Long Running)
- **Test 4.2**: File Descriptor Leaks
- **Test 4.3**: Python Reference Counting

### Phase 5: Performance Tests (`test_performance.py`)
- **Test 5.1**: GIL Contention Measurement
- **Test 5.2**: I/O-Bound Concurrency
- **Test 5.3**: Stress Test (High Thread Count)
- **Test 5.4**: Rapid Thread Creation/Destruction

### Phase 6: Python Version Compatibility Tests (`test_python_versions.py`)
- **Test 6.1**: Python 3.7 Compatibility
- **Test 6.2**: Python 3.8-3.11 Compatibility
- **Test 6.3**: Python 3.12+ Per-Interpreter GIL (Optional)

### Phase 7: Edge Cases Tests (`test_edge_cases.py`)
- **Test 7.1**: Recursive Calls
- **Test 7.2**: Daemon Threads
- **Test 7.3**: Signal Handling
- **Test 7.4**: Very Long Running Operations

## Running the Tests

### Run all threading tests:
```bash
cd build
make -j test_python
```

### Run specific phase:
```bash
cd build/tests/cython/multithread
python3 -m unittest test_concurrent_eval
```

### Run specific test:
```bash
python3 -m unittest test_concurrent_eval.Test_1_1_ConcurrentEvalCreation.test_concurrent_eval_creation_50_threads
```

### Run with verbose output:
```bash
python3 -m unittest test_concurrent_eval -v
```

## Test Utilities

### `test_threading_utils.py`
Provides shared utilities for all threading tests:
- `ThreadTestCase`: Base class with helper methods
- `MemoryMonitor`: Context manager for detecting memory leaks
- `ThreadSafetyValidator`: For recording and asserting thread safety
- Decorators: `@timeout`, `@check_memory_leaks`, `@requires_python_version`

### `helper_module.py`
External Python module with test functions called from GroundedSchemas.

## Debugging Failed Tests

### Memory Leaks
```bash
valgrind --leak-check=full --show-leak-kinds=all \
    python3 -m unittest test_concurrent_eval
```

### Race Conditions
```bash
# Build with ThreadSanitizer
cmake -DCMAKE_CXX_FLAGS="-fsanitize=thread" ..
make
# Then run tests
```

### GDB Debugging
```bash
gdb --args python3 -m unittest test_concurrent_eval.Test_1_1_ConcurrentEvalCreation.test_concurrent_eval_creation_50_threads
```

## Notes

- Tests use barriers for synchronized thread start to maximize contention
- Timeouts prevent hanging tests
- Memory monitoring detects leaks (with configurable tolerance)
- All tests should be deterministic and repeatable
