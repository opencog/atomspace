
INCLUDE_DIRECTORIES(
	${PYTHON_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
)

SET(CYTHON_FLAGS "-3" "-f" "-Wextra")

CYTHON_ADD_MODULE_PYX(storage_constructors
	"../../../storage/storage_types.pyx"
	"../../../storage/storage_types.h")

ADD_LIBRARY(storage_cython SHARED
	stub.cc
	storage_constructors.cpp
)

ADD_DEPENDENCIES(storage_cython storage_types)

TARGET_LINK_LIBRARIES(storage_cython
   ${NO_AS_NEEDED}
   atomspace
   ${PYTHON_LIBRARIES}
)

SET_TARGET_PROPERTIES(storage_cython PROPERTIES
   PREFIX ""
   OUTPUT_NAME storage)
