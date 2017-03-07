--
-- convert.sql
--
-- Conversion script to convert a version-2.0 database to version 3.0
-- The difference between these two is that support was added for
-- generic values, and that truth values are now handled with this
-- generic value mechanism.
--
-- Use at your own risk. Backup your data. You may need to fiddle
-- with the UUID for the *-TruthValueKey-*

ALTER TABLE Atoms RENAME TO Atoms_Backup;
CREATE TABLE Atoms AS SELECT * FROM Atoms_Backup;

CREATE TABLE Valuations (
    key BIGINT REFERENCES Atoms(uuid),
    atom BIGINT REFERENCES Atoms(uuid),
    type  SMALLINT,
    floatvalue DOUBLE PRECISION[],
    stringvalue TEXT[],
    linkvalue BIGINT[],
    UNIQUE (key, atom)
);

CREATE INDEX ON Valuations (atom);

CREATE TABLE Values (
    vuid BIGINT PRIMARY KEY,
    type  SMALLINT,
    floatvalue DOUBLE PRECISION[],
    stringvalue TEXT[],
    linkvalue BIGINT[] -- ELEMENT REFERENCES Values(vuid)
);


