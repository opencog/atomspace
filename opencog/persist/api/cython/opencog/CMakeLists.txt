
INCLUDE_DIRECTORIES(
	${PYTHON_INCLUDE_DIRS}
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
)

SET(CYTHON_FLAGS "-3" "-f" "-Wextra")

CYTHON_ADD_MODULE_PYX(storage
	"storage_types.pyx"
	storage_types
	""
)
# ADD_DEPENDENCIES(storage storage_types)
