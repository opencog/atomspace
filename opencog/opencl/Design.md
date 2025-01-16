Design Notes
------------
Notes about OpenCL interfaces and how they impact Atomese design.

* Vectors will need a corrsponding (private) `cl::Buffer` object
  (per vector).  The `cl::Buffer()` ctor requires a `cl::Context`.

* The kernel itself needs only a `cl::Program` which holds the
  actual kernel code.

* Kernels are executed by placing them onto a `cl::CommandQueue`.
  This queue takes both a `cl::Context` and also a `cl::Device` 
  in it ctor. Kernels are exec async.

* Obtaining results from the exec needs a `cl::Event`, created when
  the kernel is enqueued.
