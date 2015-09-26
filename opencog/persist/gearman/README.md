=Distributed processing with Gearman

Implementation details of distributed processing for opencog with
Gearman.

==Overview
This implementation allows opencog cogserver to distribute processing
between multiple processes or machines, using the Gearman distributed
job contol system.  Jobs are defined by writing short guile scheme
snippets, which are sent to to the German workers.  To communitcate
the results of the computation, the scheme programs should save any
desired/relevant atoms to the PostgreSQL backend.

==Install
* To compile with Gearman support, you need to install `libgearman-dev`.

* To actually run the system 9and run the unit test!) you must also
install `gearman-job-server` and make sure that it is running:
`sudo service gearman-job-server start`.


==Usage
From the guile shell:
```
   (use-modules (opencog dist-gearman))
```
1. `(set-slave-mode “master-ip-address” “slave-id-name”)` - enter a
   particular thread on cogserver to blocking single slave thread

2. `(set-master-mode)`  - This causes all slave threads on a local
   cogserver to not process any new queries and exit as soon as
   currently running query is finished

3. `(dist-run-scm “scheme-code-returning-handle” “master-id-name” #t)`
    - this makes a cogserver thread send a scheme code to run on slave,
    assuming the code returns a atom handle on completion. The master
    gets the handle uuid when slave finishes the processing and pushing
    the resulting atoms to postgres backing store

4. New signature type registered in scheme primitives header for master
   callback as U_SSB, returns UUID and takes scheme code string, master
   id string, boolean


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
