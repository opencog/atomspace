# Distributed evaluation of Scheme code with Gearman

This guile module allows scheme code to be evaluated on
other (non-local, internet-connected, distributed) servers.
Under the covers, it uses Gearman to perform the distribution.

## Overview
AtomSpace scheme users can distribute the evaluation of scheme code
across multiple systems using the Gearman distributed job control
system.  Jobs are defined by writing (short) scheme programs, which
are sent to the Gearman workers.  Results are communicated back as
scheme strings.

See https://en.wikipedia.org/wiki/Gearman for a description of the
general framework.

## Critique
As currently implemented, this is a half-baked, incomplete, and mostly
untested idea.

* There is no job management and no network control.  Currently,
  there is no way for scheme job servers to be started on remote
  machines. Servers have to be started and managed by hand.

* Load-balancing concepts are unclear and undocumented.

* The current API only allows workers to be started once. Once the
  worker threads exit, they cannot ever be restarted again.

* The code for having multiple threads handling work is untested,
  and is probably buggy.  On the other hand, it is not clear why
  one would want to have more than one thread handling work, anyway,
  so this is probably OK.

* The code is written in such a way that it depends on the atomspace.
  There is no reason for this: this could have just as easily been a
  generic distributed scheme evaluation infrastructure that does not
  require or assume the atomspace.  In other words, the workers should
  be redesigned to run the generic scheme evaluator, instead of the
  AtomSpace evaluator. The return value of a worker MUST be an atom,
  other scheme type are currently not supported.

* Security best practices is unclear.  Are users supposed to set up
  a VPN? How is authentication and authorization handled? Under what
  conditions can a worker trust a client?

* Major processing probably requires that all workers establish a
  connection to a shared pesistant SQL storage backend.  There is
  no documentation showing how this could best be done.

## Prerequisites
* The `libgearman-dev` package must be installed before compiling.

* To actually run the system (and run the unit test!) you must also
install `gearman-job-server` and make sure that it is running:
```
sudo service gearman-job-server status
```
and, if necessary,
```
sudo service gearman-job-server start
```

## Usage
This guile module can be loaded as:
```
   (use-modules (opencog) (opencog dist-gearman))
```
The API provides three routines. Two are used to manage the workers,
and a third function to distribute expressions.

```
(start-work-handler “gearmand-ip-address” “worker-id”)
```

   Converts the current thread into a work handler. This will enter
   an infinite loop, which queries the indicated gearmand job server
   for work requests. The current thread will thenceforth process
   work requests, blocking if there is no work.  This function will
   return only if there is a Gearman error, or if the exit flag,
   below, is set.

```
(exit-all-workers)
```
   This causes all work handler threads to exit their dispatch loops.
   After this is called, no new queries will be accepted. Any pending
   work being performed in any worker threads will continue to run until
   completion, and the results will be returned to the client. After
   completion, the worker dispatch loop will exit, and callers of
   `start-work-handler`, above, will return.

   This call is effective only when executed within the same process
   in which the `start-work-handler` function was called.  That is,
   it can only be used to manage the local worker threads.

```
(dist-eval “scheme-expr” “worker-id-name”)
```
   This sends the indicated scheme expression to be evaluated on
   the indicated worker. It will block until a reply is received.
   The reply is expected to be a string.

## Implementation status

Here's what you can currently do:

1. A particular thread can be put into worker mode, giving the IP
   address of the gearmand job server that should be contacted to
   get work requests.

2. A scheme expression can be sent to a gearmand server, for evaluation.
   The sender will block until the evaluation is complete, and a
   result is returned.

3. The worker will fetch scheme expressions from the gearmand server,
   evaluate them, and return the result, as a string.

The current implementation has only been tested on a single machine
with both worker and client threads within the same process.
