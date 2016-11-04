=Distributed evaluation of Scheme code with Gearman

This guile module allows scheme code to be evaluated on
other (non-local, internet-connected, distrubued) servers.
It uses Gearman under the covers to accomplish this.

==Overview
AtomSpace scheme users can distribute processing across multiple
systems using the Gearman distributed job contol system.  Jobs are
defined by writing (short) scheme programs, which are sent to the
Gearman workers.  Results are communicated back as scheme strings.

==Critique
This is a half-baked, incompletely implemented idea.

==Prerequisites
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


==Usage
From the guile shell:
```
   (use-modules (opencog dist-gearman))
```
1. `(start-work-handler “gearmand-ip-address” “worker-id”)`
   Converts the current thread into a work handler. This will enter
   an infinite loop, which queries the indicated gearmand job server
   for work requests. The current thread will thenceforth process
   work requests, blocking if there is no work.  The inf loop will
   exit upon error, or if the exit flag, below, is set.

2. `(exit-all-workers)`
   This causes all work handlers to exit thier dispatch loops.
   After this is called, no new queries will be accepted. Any
   pending work being performed in any running threads will be
   completed, and the results returned, before that thread's
   loop is exited.

3. `(dist-eval “scheme-code” “worker-id-name”)`
    This sends the indicated block of scheme code to be evaluated on
    the indicated worker.

== Implenetation status

Here's what you can curently do:

1. A particular thread can be put into worker mode, giving the IP
   address of the gearmand job server that should be contacted to
   get work requests.

2. A scheme expression can be sent to a gearmand server, for evalaution.
   The sender will block until the evaluation is complete, and a
   result is returned.

3. The worker will fetch scheme expressions from the gearmand server,
   evaluate them, and return the result, as a string.

The current implementation has only been tested on a single machine
with both worker and master threads within the same process.
