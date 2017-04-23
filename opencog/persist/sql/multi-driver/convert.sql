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
-- You WILL need to fiddle with the type number chosen for the
-- SimpleTruthValue and CountTruthValue, below.

-- Backup the affectede table
ALTER TABLE Atoms RENAME TO Atoms_Backup;
CREATE TABLE Atoms AS SELECT * FROM Atoms_Backup;

CREATE UNIQUE INDEX ON Atoms(uuid);
CREATE UNIQUE INDEX ON Atoms(type, name);
CREATE UNIQUE INDEX ON Atoms(type, outgoing);


-- Create the new, needed tables
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

-- Create the TruthValueKey.
-- You may need to change the uuid value to some (any) unused uuid
-- Whatever you pick here, must also be used as the  key, below.
INSERT INTO Atoms (uuid, space, type, height, name) VALUES
	(1, 1,
	(SELECT type FROM TypeCodes WHERE typename = 'PredicateNode'),
	0, '*-TruthValueKey-*');

-- 'key' must be exactly the same as uuid above.
--
-- In the old (version 2.0) tables:
-- tv_type == 1 is SimpleTV
-- tv_type == 2 is CountTV
-- No other TV types were ever used/supported.
--
-- The new types are:
-- type == 6 == SimpleTruthValue
-- type == 7 == CountTruthValue
--
-- The above should be correct for the current versions;
-- however, YMMV, so check to make sure. The below copies
-- these two types.

-- hackery, to not re-write the typecodes table.
INSERT INTO typecodes (type, typename) VALUES
	(176, 'SimpleTruthValue');

INSERT INTO typecodes (type, typename) VALUES
	(177, 'CountTruthValue');

INSERT INTO Valuations
	(SELECT 1 AS key,  -- 1 here is the predicate node
		uuid AS atom,
		176 as type,
		ARRAY[stv_mean, stv_confidence] as floatvalue
		FROM Atoms WHERE tv_type = 1);

INSERT INTO Valuations
	(SELECT 1 AS key, -- 1 here is the predicate node
		uuid AS atom,
		177 as type,
		ARRAY[stv_mean, stv_confidence, stv_count] as floatvalue
		FROM Atoms WHERE tv_type = 2);

-- Drop the columns we no longer use.
ALTER TABLE Atoms DROP COLUMN tv_type;
ALTER TABLE Atoms DROP COLUMN stv_mean;
ALTER TABLE Atoms DROP COLUMN stv_confidence;
ALTER TABLE Atoms DROP COLUMN stv_count;

-- That's all, folks!  The result should now work with the version 3.0
-- postgres drivers.
