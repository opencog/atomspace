
Automapping of SQL DB's
=======================

Per request of Mike Duncan. Map the DB

https://ftp.flybase.net/releases/FB2022_06/psql/FB2022_06.sql.gz

into the AtomSpace, using the schema given at

http://gmod.org/wiki/Chado

Attempt #1
----------
For each row in tablename, create a conventional EvaluationLink.
It contains *only* the columns that are not foreign keys:
```
  Evaluation
     Predicate "tablename"
     List
        Concept "column 1" ; If there are strings
        Concept "column 2"
        ...
        NumberNode NNN  ; if there are numbers.
```

For each row in tablename having a column that is a foreign key:
```
   Evaluation
      Predicate "host tablename"
      Predicate "foreign key"
      Evaluation 
         Predicate "target tablename"
         List
             Concept ...
```

That's it. Is there more?



