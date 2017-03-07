--
-- convert.sql
--
-- Conversion script to convert a version-2.0 database to version 3.0
-- The difference between these two is that support was added for
-- generic values, and that truth values are now handled with this
-- generic value mechanism.
--
-- Use at your own risk. Backup your data.
--
-- THIS HAS NEVER BEEN USED OR TESTED. IT MAY DESTROY YOUR DATA.
--
-- You may need to fiddle with the UUID for the *-TruthValueKey-*
--

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

-- Change uuid to some, any unused uuid
INSERT INTO Atoms (uuid, space, type, height, name) VALUES
	(1, 1, 
	SELECT type FROM TypeCodes WHERE typename = "PREDICATE_NODE",
	0, "*-TruthValueKey-*");

-- 'key' must be exactly the same as uuid above.
INSERT INtO Valuations (key, atom, type, floatvalue) VALUES
	(1, 
	SELECT uuid FROM Atoms,
	16,
	{0,0,0}
	); 

-- Drop the columns we no longer use.
ALTER TABLE Atoms DROP COLUMN tv_type;
ALTER TABLE Atoms DROP COLUMN stv_mean;
ALTER TABLE Atoms DROP COLUMN stv_confidence;
ALTER TABLE Atoms DROP COLUMN stv_count;
