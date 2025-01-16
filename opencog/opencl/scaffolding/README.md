
Command line tools:
* `clinfo` to print OpenCL hardware

Hello-world examples from the inter-tubes:

* Hello-world OpenCL demo from 2019:
  https://github.com/cqcallaw/ocl-samples/

* Many different examples from 2017; all for Apple:
  https://github.com/rsnemmen/OpenCL-examples/
  including the most basic hello-world:
  https://github.com/rsnemmen/OpenCL-examples/blob/master/Hello_World/hello.c

* Tutorial from 2010, still seems mostly correct
  https://www.codeproject.com/articles/92788/introductory-tutorial-to-opencl

Resources:
* https://www.khronos.org/opencl/resources

Languages:
* The OpenCL kernel language is based on C99
* SYCL is based on c++17 "single source" (SIMD ??)
* Generic C++ for OpenCL:
  https://github.com/KhronosGroup/OpenCL-Guide/blob/main/chapters/cpp_for_opencl.md

Non-obvious concepts:
* The kernel can be executed on a 1D, 2D, or 3D domain of indexes that
  execute in parallel.
* The total number of elements (aka indexes, aka work items) in the
  launch domain is called the global work size.
* Individual work-items can be grouped into work-groups, for inter-item
  communications. These are of "local work size".
