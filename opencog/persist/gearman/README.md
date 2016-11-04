=Distributed processing with Gearman

AtomSpace processing can be distributed across multiple servers.
This is accomplished by using Gearman under the covers.

==Overview
AtomSpace users can distribute processing across multiple systems
using the Gearman distributed job contol system.  Jobs are defined
by writing (short) scheme programs, which are sent to to the Gearman
workers.  Results are communicated back as scheme strings.

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

3. `(dist-run-scm “scheme-code-returning-handle” “master-id-name”)`
    - this makes a cogserver thread send a scheme code to run on slave,
    assuming the code returns a atom handle on completion. The master
    gets the handle uuid when slave finishes the processing and pushing
    the resulting atoms to postgres backing store


== Implenetation status
Currently working:
1. A particular thread of cogserver is put in slave mode giving the ip
   of master (on same machine this will be localhost)

2. From another machine or cogserver thread  we send a scheme program
   string to the slave. The call blocks until the slave finishes and
   returns.

3. The slave takes scheme code from master which is supposed to return
   a handle as a result. [the scheme code should push atom results to
   backing store if required]

4. Slave runs the code in its thread and at completion gets uuid of the
   resulting handle and sends it back to master

5. Master returns the uuid of the handle to the caller. [in case atoms
   were supposed to be pushed to backing store, retrieve from backing store]

Note: Current implementation has only been tested on a single machine
with slave and master threads on same cogserver.
