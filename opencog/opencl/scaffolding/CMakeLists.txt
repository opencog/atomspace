
INCLUDE_DIRECTORIES(${OPENCL_INCLUDE_DIRS})

# Build scaffolding tool
ADD_EXECUTABLE(show-ocl-hw show-ocl-hw.cc scaffolding.cc)
ADD_EXECUTABLE(run-hello-world run-hello-world.cc scaffolding.cc)
ADD_EXECUTABLE(run-vec-mult run-vec-mult.cc scaffolding.cc)
ADD_EXECUTABLE(run-vec-flow run-vec-flow.cc scaffolding.cc)

# TARGET_LINK_LIBRARIES(show-ocl-hw ${OPENCL_LIBRARIES})
TARGET_LINK_LIBRARIES(show-ocl-hw OpenCL::OpenCL)
TARGET_LINK_LIBRARIES(run-hello-world OpenCL::OpenCL)
TARGET_LINK_LIBRARIES(run-vec-mult OpenCL::OpenCL)
TARGET_LINK_LIBRARIES(run-vec-flow OpenCL::OpenCL)
