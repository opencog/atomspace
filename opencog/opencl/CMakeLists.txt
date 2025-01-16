
INCLUDE_DIRECTORIES(${OPENCL_INCLUDE_DIRS})

# Build scaffolding tool
ADD_EXECUTABLE(scaffolding scaffolding.cc)

# TARGET_LINK_LIBRARIES(scaffolding ${OPENCL_LIBRARIES})
TARGET_LINK_LIBRARIES(scaffolding OpenCL::OpenCL)
