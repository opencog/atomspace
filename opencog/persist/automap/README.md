
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
			Concept "column 1" ; If column type is a string
			Concept "column 2"
			...
			NumberNode NNN	; If column type is a number.
```

For each row in tablename having a column that is a foreign key:
```
	Evaluation
		Predicate "host tablename . foreign key"
		List
			Evaluation ;;; row in the host table
				Predicate "host tablename"
				List
					Concept ...

			Evaluation	;; row in the target table.
				Predicate "target tablename"
				List
					Concept ...
```

That's it. Is there more?

-----------------------------------------------------------------------
