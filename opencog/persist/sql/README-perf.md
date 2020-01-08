Performance diary and status
============================
In March 2014, 10.3M atoms were loaded in about 20 minutes wall-clock
time, 10 minutes of OpenCog-server CPU time.  This works out to about
500K atoms/minute, or 9K atoms/second.  The resulting cogserver required
about 10GBytes of RAM, which works out to about 1KByte/atom average.
The loaded hypergraphs were all EvaluationLinks, viz:
```
   EvaluationLink  (w/non-trivial TV)
      SomeNode
      ListLink
         WordNode  (w/ non-trivial TV)
         WordNode  (w/ non-trivial TV)
```

The above measurements were made on a busy server that was doing many
other CPU & RAM intensive things; there was no performance tuning of
the Postgres server.  A section below explains how to performance tune
the Postgres server for better results.  The above was also done through
the scheme interface; since then, garbage collection has been tuned a
little bit, and so RAM usage should drop a bit.

In June 2018, it took 3500 seconds wall-clock time to load 25924129
atoms, for a rate of 7407 atoms/sec. Of these atoms, approximately half
had non-trivial values associated with them. The cogserver process used
38GB of RAM, for about 1.46KBytes/atom.

Storing portions of this dataset proceeds at about 1K atoms/sec. This
is for updating the values on the atoms, only; no actual atoms are
stored (i.e. the atoms are already in the database; as are the values;
the values are being updated).  Precisely, there were 6239904 stores
in 6561 seconds, wall-clock time, for a rate of 951 Atoms/second.

Store is performed by multiple parallel threads in the backend, each
taking to a distinct instance of Postgres; thus, it appears that
Postgres is the primary bottleneck. Certainly, Postgres seems to be the
primary consumer of CPU time, using a combined 2x more CPU than the
AtomSpace. i.e. for every cpu-sec burned by the AtomSpace, the six
different Postgres instance burn up two cpu-secs.

It is not at all obvious how to improve either load or store performance.


Experimental Diary & Results
============================
Diary entries from June 2008

Store performance
-----------------
This section reviews the performance for storage of data from OpenCog
to the SQL server (and thence to disk).  Performed in 2008, on a typical
Intel desktop that was new in 2004. Viz. under two GhZ, and 4GB RAM.

First run with a large data set (save of 1564K atoms to the database)
was a disaster.  Huge CPU usage, with 75% of CPU usage occurring in the
kernel block i/o layer, and 12% each for the OpenCog and postgres times:
```
   112:00 [md4_raid1] or 4.3 millisecs per atom
   17 minutes for postgres, and OpenCog, each. or 0.66 millisecs per atom
   1937576 - 1088032 kB = 850MBytes disk use
```
Experiment: is this due to the bad design for trying to figure whether
"INSERT" or "UPDATE" should be used? A local client-side cache of the
keys in the DB seems to change little:
```
   CPU usage for postgres 11:04  and OpenCog 10:40 and 112:30 for md
```
So this change drops postgres server and OpenCog CPU usage
significantly, but the insane kernel CPU usage remains.

The above disaster can be attributed to bad defaults for the postgres
server. In particular, sync to disk, while critical for commercial
database use, is pointless for current use. Also, buffer sizes are much
too small. Edit postgresql.conf and make the following changes:
```
   shared_buffers = default was 24MB, change to 384MB
   work_mem = default was 1MB change to 32MB
   fsync = default on  change to off
   synchronous_commit = default on change to off
   wal_buffers = default 64kB change to 512kB
   commit_delay = default 0 change to 10000 (10K) microseconds
   ssl = default true change to false
```
Restarting the server might lead to errors stating that max shared mem
usage has been exceeded. This can be fixed by:
```
   vi /etc/sysctl.conf
   kernel.shmmax = 440100100
(save file contents, then)
   sysctl -p /etc/sysctl.conf
```
After tuning, save of data to empty DB gives result:
```
   cogserver = 10:45 mins = 0.41  millisecs/atom (2.42K atoms/sec)
   postgres  =  7:32 mins = 0.29  millisecs/atom (2.65K atoms/sec)
   md        =  0:42 mins = 0.026 millisecs/atom (37K atoms/sec)
```
Try again, dropping the indexes on the atom and edge tables. Then,
after loading all atoms, rebuild the index. This time, get
```
   cogserver = 5:49 mins = 0.227 millisecs/atom (4.40K atoms/sec)
   postgres  = 4:50 mins = 0.189 millisecs/atom (5.30K atoms/sec)
```
Try again, this time with in-line outgoing sets. This improves
performance even further:
```
   cogserver = 2:54 mm:ss = 0.113 millisecs/atom (8.83K atoms/sec)
   postgres  = 2:22 mm:ss = 0.092 millisecs/atom (10.82K atoms/sec)
```
Try again, compiled with -O3, storing to an empty table, with
no indexes on it (and with in-line outgoing sets):
```
   cogserver = 2:40 mm:ss
   postgres  = 2:16 mm:ss
```
Try again, compiled with -O3, storing to empty table, while holding
the index on tables (and with in-line outgoing sets).
```
   cogserver = 2:51 mm:ss
   postgres  = 2:06 mm:ss
```
Apparently, the problem with the indexes has to do with holding them
for the edge table; when there's no edge table, then there's no index
issue!?

Try again, compiled with -O3, saving to (updating) a *full* database.
(i.e. database already has the data, so we are doing UPDATE not INSERT)
```
   cogserver = 2:19 mm:ss
   postgres  = 4:35 mm:ss
```
Try again, using UnixODBC instead of iODBC, to empty table, withOUT
index tables (and -O3 and in-lined outgoing):
```
   cogserver = 2:36 mm:ss
   postgres  = 2:13 mm:ss
```
It appears that UnixODBC is essentially identical to iODBC performance
```
begin; commit;
use analyze;
use prepare;
```

Loading performance
-------------------
Loading performance. Database contains 1564K Atoms, and 2413K edges.
CPU usage:
2:08 postgres = 82 microsecs/atom (12.2K atoms/sec)
similar to for OpenCog, but then AtomTable needs to reconcile, which
takes an additional 8:30 minutes to attach incoming handles!!

Conclude: database loading would be much faster if we loaded all of
the atoms first, then all of the lowest-height links, etc.  This assumes
that OpenCog is strictly hierarchically structured. (no "crazy loops")

After implementing height-structured restore, get following, loading
from a "hot" postgres instance (had not been stopped since previous
use, i.e. data should have been hot in RAM):
```
  cogserver = 2:36 mm:ss = 0.101 millisecs/atom (9.85K atoms/sec)
  postgres  = 1:59 mm:ss = 0.077 millisecs/atom (12.91K atoms/sec)
```
The dataset had 357162 Nodes, 1206544 Links at height 1

After a cold start, have
```
  cogserver = 2:32 mm:ss
  postgres  = 1:55 mm:ss
```
Appears that there is no performance degradation for cold-starts.

Note also: cogserver CPU usage is *identical* to its CPU usage when
loading XML! Hurrah! Also, see below: RAM usage is significantly
reduced; apparently, the reading of XML results in very bad memory
fragmentation.

Implement in-line edges, instead of storing edges in an outboard table.
```
  cogserver = 41 seconds = 26.7 microsecs/atom (37.5K atoms/sec)
  postgres  =  7 seconds =  4.56 microsecs/atom (219K atoms/sec)
```
Turn on -O3 optimization during compile ... all previous figures
were without *any* optimization. Now get
```
  cogserver = 24 seconds = 15.6 microsecs/atom (64.0K atoms/sec)
  postgres  = 11 seconds
```
Much much better!
```
10.78
23.15
```

***The End***
