Implementation details of distributed processing for opencog with gearman.
Overview:
This implementation allows opencog cogserver to distribute its processing between multiple threads and/or machines using gearman. The basic infrastructure used was gearman, scheme primitives in atomspace library and postgres backing store. The functionality is added into atomspace as an extension module and adds libgearman dependency to it.


What is added:
I added three scheme primitives (use-modules (opencog dist-gearman))
1. (set-slave-mode “master-ip-address” “slave-id-name”) - enters a particular thread on cogserver to blocking single slave thread
2. (set-master-mode)  - This causes all slave threads on a local cogserver to not process any new queries and exit as soon as currently running query is finished
3. (dist-run-scm “scheme-code-returning-handle” “master-id-name” #t) - this makes a cogserver thread send a scheme code to run on slave, assuming the code returns a atom handle on completion. The master gets the handle uuid when slave finishes the processing and pushing the resulting atoms to postgres backing store
4. New signature type registered in scheme primitives header for master callback as U_SSB, returns UUID and takes scheme code string, master id string, boolean


Working:
1. a particular thread of cogserver is put in slave mode giving the ip of master (on same machine this will be localhost)
2. from another machine or cogserver thread  we send a scheme program string to the slave. The call blocks until the slave finishes and returns.
3. The slave takes scheme code from master which is supposed to return a handle as a result. [the scheme code should push atom results to backing store if required]
4. Slave runs the code in its thread and at completion gets uuid of the resulting handle and sends it back to master
5. master returns the uuid of the handle to the caller. [in case atoms were supposed to be pushed to backing store, retrieve from backing store]

Note: Current implementation has only been tested on a single machine with slave and master threads on same cogserver
