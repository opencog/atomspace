
INCLUDE_DIRECTORIES(${OPENCL_INCLUDE_DIRS})

# Build scaffolding tool
ADD_EXECUTABLE(show-ocl-hw show-ocl-hw.cc scaffolding.cc)

# TARGET_LINK_LIBRARIES(show-ocl-hw ${OPENCL_LIBRARIES})
TARGET_LINK_LIBRARIES(show-ocl-hw OpenCL::OpenCL)
