SQLAtomStorage
==============
Persistent Atom storage, SQL-backed.

Atoms and Values are saved to, and restored from, an SQL DB using
one of the available database drivers. Currently, the postgres
native libpq-dev API and the ODBC API are supported. Note that
libpq-dev is about three times faster than ODBC.

Atoms are identified by means of unique ID's (UUID's), which are
correlated with specific in-RAM atoms via the TLB.

File guide
----------
Some wrappers for storage drivers:
* `llapi.cc`, `llapi.h`         -- Low-level database driver API
* `ll-pg-cxx.cc`, `ll-pg-cxx.h` -- LLAPI for PostgreSQL "libpq" driver.
* `odbcxx.cc`, `odbcxx.h`       -- LLAPI for ODBC driver

Primary database API definition:
* `atom.sql`             --  Postgresql Database Table definitions
* `SQLAtomStorage.cc`, `SQLAtomStorage.h` -- Main Class definition
* `SQLPersistSCM.cc`, `SQLPersistSCM.h`   -- Guile interfaces

Implementation files:
* `SQLAtomDelete.cc` -- Single atom deletion 
* `SQLAtomLoad.cc`   -- Single atom load-from-SQL
* `SQLAtomStore.cc`  -- Single atom save-to-SQL
* `SQLBulk.cc`       -- Load and Store of multiple atoms, in bulk.
* `SQLResponse.h`    -- Row+Column to Atom conversion utilities
* `SQLSpaces.cc`     -- AtomsSpace load and store
* `SQLTypeMap.cc`    -- Atom types and type-names
* `SQLUUID.cc`       -- UUID management
* `SQLValues.cc`     -- Save and restore of Values
