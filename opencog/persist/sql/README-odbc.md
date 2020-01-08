SQL Persist - ODBC setup
========================
The current codebase supports ODBC access to an SQL database. However,
the use of the ODBC driver is discouraged, because it is more than twice
as slow as the native (postgres) bindings.  In addition, ODBC presents
a significantly more complex setup and configuration challenge.  So,
unless you really need it or really like it, you should avoid using ODBC.
But if you have to, the instructions are below.

Install
-------
First, download and install UnixODBC devel packages.  Do NOT use
IODBC, it fails to support UTF-8!  It's also buggy when more than a few
100K atoms need to be fetched.

```
  sudo apt-get install unixodbc-dev odbc-postgresql
  cmake
  make
```

ODBC Device Driver Setup
------------------------
After install, verify that `/etc/odbcinst.ini` contains the stanza
below (or something similar).  If it is missing, then edit this file
(as root) and add the stanza.  Notice that it uses the Unicode drivers,
and NOT the ANSI (ASCII) drivers.  OpenCog uses Unicode!

```
    sudo vi /etc/odbcinst.ini &
```

```
    [PostgreSQL Unicode]
    Description = PostgreSQL ODBC driver (Unicode version)
    Driver      = psqlodbcw.so
    Setup       = libodbcpsqlS.so
    Debug       = 0
    CommLog     = 0
```

The above stanza associates the name `PostgreSQL Unicode` with a particular
driver. This name is needed for later steps.  Notice that this is a quite
long name, with spaces!  You can change the name, (e.g. to shorten it) if
you wish, however, it **MUST** be consistent with the name given in the
`.odbc.ini` file (explained below in 'ODBC Setup, Part the Second').

MySQL users need the stanza below; the `/etc/odbcinst.ini` file can
safely contain multiple stanzas defining other drivers.

WARNING: MySQL is currently not supported. Anyway, if you wanted to
mess with it, then add the below:
```
    [MySQL]
    Description = MySQL driver
    Driver      = libmyodbc.so
    Setup       = libodbcmyS.so
    CPTimeout   =
    CPReuse     =
```

ODBC Dataset Configuration
--------------------------
Edit `~/.odbc.ini` in your home directory to add a stanza of the form
below. You MUST create one of these for EACH repository you plan to
use! The name of the stanza, and of the database, can be whatever you
wish. The name given for `Driver`, here `PostgreSQL Unicode`, **must**
match a stanza name in `/etc/odbcinst.ini`.  Failure to have it match
will cause an error message:

```
Can't perform SQLConnect rc=-1(0) [unixODBC][Driver Manager]Data source name not found, and no default driver specified
```

Note that below specifies a username and a password. These should NOT
be your regular unix username or password!  Make up something else,
something completely different! These two will be the username and the
password that you use when connecting from the atomspace, with the
`sql-open` command (below).

Pay special attention to the name given for the `Database`.  This should
correspond to a database created with postgres. In the examples above,
it was `mycogdata`.

IMPORTANT: MAKE SURE THERE ARE NO SPACES AT THE START OF EVERY LINE!
           IF YOU COPY-PASTE THE TEXT BELOW YOU WILL HAVE SPACES!
           REMOVE THEM! OR YOU WILL ALSO GET THE ERROR ABOVE!

```
   [mycogdata]
   Description       = My Favorite Database
   Driver            = PostgreSQL Unicode
   Database          = mycogdata
   Servername        = localhost
   Port              = 5432
   Username          = opencog_user
   Password          = cheese
```
