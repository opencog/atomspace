Design Notes
------------
Notes about OpenCL interfaces and how they impact Atomese design.

* Vectors will need a corrsponding (private) `cl::Buffer` object
  (per vector).  The `cl::Buffer()` ctor requires a `cl::Context`.
  Some `cl::Buffer` ctors don't require a context; these all use
  the default context instead (seems like a bad idea, for us...)

* Alternative is to use SVM "Shared Virtual Memory", The ctors
  also need `cl::Context` and if not supplied, uses the default.

* The kernel itself needs only a `cl::Program` which holds the
  actual kernel code.

* Kernels are executed by placing them onto a `cl::CommandQueue`.
  This queue takes both a `cl::Context` and also a `cl::Device` 
  in it ctor. Kernels are exec async.

* Obtaining results from the exec needs a `cl::Event`, created when
  the kernel is enqueued.

Design alternatives:
* Use `GroundedProceedureNode`s to wrap kernels. Old-style, yucky.
  Why yucky? Because its stateless: besides the string name of the
  node, nothing is stored in the atom. There's no open/close phasing.
  Its just a function call. Maps poorly onto stateful I/O.

* Use `StorageNode`s to wrap kernels. This presents other issues.
  First, traditional `StorageNodes` were meant to send/recieve atoms,
  while here, we need to send/receive vectors represented as
  `FloatValue`s.  Maybe other kinds of vectors, but for now,
  `FloatValue`s.  The other problem is that the `StorageNode`
  API sits outside of Atomese proper, and uses instead a collection
  of calls `cog-open` and `cog-close` to manage connection state.
  Which is adequate for the intended use, but leaves something to be
  desired.

* Create something new, inspired by `BackingStore`. This removes some
  of the issues with StorageNodes, but leaves others in place. One
  is that, again, the API sits outside of Atmoese. The other is that
  this becomes yet another engineered solution. Of course, engineered
  solutions are inescapable, but... for example: the traditional unix
  open/close/read/write 4-tuple provides an abstraction that works very
  well for many I/O tasks: open/close to manage the channel, and
  read/write to use it. But the OpenCL interfaces do not map cleanly
  to open/close/read/write. They use a more compex structure.

The abstraction I'm getting is this: open near point, open far point,
select far-point message receipient, send message. That mapping would
have psuedocode like so:
```
   open_near_point() {
      Create cl::Context and retain pointer to it
   }

   open_far_point() {
      Using cl::Context from near point, and externally specified
      cl::Device, create a cl::CommandQueue. Retain pointer to the
      cl::CommandQueue. Its the commo handle.
   }

   select_recipient() {
      Create a cl::Event as the commo handle, retain pointe to it.
      Use the externally supplied cl::Program, retain pointer to it.
   }

   write() {
      Using the selected recipient, create a cl:Kernel.
      Call cl::Kernel::setArgs() in th kernel to establish the connetion.
      Call cl::CommandQueue::enqueueNDRangeKernel() to perform send
   )

   read() {
      Using the selected recipient, wait on cl::Event
   }
```
