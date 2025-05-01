#
# Need to use -fno-strict-aliasing when compiling cython code, in order
# to avoid nasty compiler warnings about aliasing.  Cython explicitly
# performs aliasing, in order to emulate python object inheritance.
# See, for example, https://groups.google.com/forum/#!topic/cython-users/JV1-KvIUeIg
#
# Also suppress unused function warnings for static functions, since
# Cython generates a static function that it puts in a header which
# causes this warning to appear whenever anyone includes the header
# and doesn't use all the functions.
#
SET(CMAKE_CXX_FLAGS
	"${CMAKE_CXX_FLAGS} -fno-strict-aliasing -Wno-unused-function")

ADD_SUBDIRECTORY (opencog)
ADD_SUBDIRECTORY (executioncontext)

INCLUDE_DIRECTORIES(
	${Python3_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
)

# Python Module init
FILE(MAKE_DIRECTORY opencog)
FILE(COPY opencog/__init__.py DESTINATION opencog)

ADD_LIBRARY(PythonEval
	PythonEval.cc
)

ADD_DEPENDENCIES(PythonEval py_atomspace_header atomspace_cython type_constructors)

TARGET_LINK_LIBRARIES(PythonEval
	pattern
	execution
	atomflow
	atombase
	atomspace
	executioncontext
	${Python3_LIBRARIES}
	${Boost_FILESYSTEM_LIBRARY}
	${Boost_SYSTEM_LIBRARY}
)

# We need to get the name of the actual python library in order
# to give it to dlopen() to open and load it. Right now,
# Python3_LIBRARIES is set to just one library, grant total.
# If there are ever more than one, this will break.
GET_FILENAME_COMPONENT(PYLIBNAME ${Python3_LIBRARIES} NAME)
TARGET_COMPILE_DEFINITIONS(PythonEval PRIVATE PYLIBNAME="${PYLIBNAME}")

INSTALL (TARGETS PythonEval EXPORT AtomSpaceTargets DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

INSTALL (FILES
	PythonEval.h
	PyIncludeWrapper.h
	DESTINATION "include/opencog/cython"
)

# ----------------------------------------------------------------
# The scheme wrapper for PythonEval. Allows python snippets to be
# called from scheme.
#
ADD_DEFINITIONS(-DPYTHON_ROOT="${PYTHON_ROOT}")
ADD_LIBRARY(PythonSCM
	PythonSCM.cc
)
ADD_DEPENDENCIES(PythonSCM PythonEval)

TARGET_LINK_LIBRARIES(PythonSCM
	PythonEval
	smob
)
ADD_GUILE_EXTENSION(SCM_CONFIG PythonSCM "opencog-ext-path-python-scm")

ADD_GUILE_MODULE (FILES
	opencog/python.scm
	DEPENDS PythonSCM
#	COMPILE
)

INSTALL (TARGETS PythonSCM
	EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)
