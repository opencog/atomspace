--
-- atom.sql
-- Version 3.0 of the Postgres database schema for the AtomSpace.
--
-- Changes since version 2.0:
--   * Added support for generic values.
--   * TruthValues now treated as generic Values.
--
-- Changes since version 1.0:
--   * Added support for multiple atomspaces.
--
-- -----------------------------------------------------------
-- Table showing inheritance relationship between atomspaces.
-- Atomspaces have UUID's identifying them. Top-level atomspaces have
-- a UUID of zero.  Otherwise, all atomspaces have some (other)
-- atomspace as a parent.

CREATE TABLE Spaces (
    space  BIGINT PRIMARY KEY,
    parent BIGINT
);

-- Space 0 is used primarily (only) in testing of raw atoms.
INSERT INTO Spaces VALUES (0,0); -- no space defined; raw atoms.
INSERT INTO Spaces VALUES (1,1); -- default root


-- -----------------------------------------------------------
-- An SQL table representation for OpensCog Atoms
--
-- This contains both the Node and Link types; having a single table
-- to contain both seemed easier than having two tables, one for Nodes
-- and one for Links.  In particular, forcing the UUID to be unique is
-- much easier when there is only one table.

CREATE TABLE Atoms (
    -- The uuid maps to the atom handle.
    -- Must be unique, and must be non-null. Ergo, primary key.
    -- Use 64-bit int to match the C++ range.
    uuid    BIGINT PRIMARY KEY,

    -- The atomspace that this atom belongs to.
    space BIGINT REFERENCES spaces(space),

    -- Atom type, e.g. Link, Node, etc.
    type  SMALLINT,

    -- distance from this link to farthest Node in the outgoing
    -- set of this atom.
    -- height of Nodes is by definition zero.
    -- height of Links containing only nodes is one, etc.
    height SMALLINT,

    -- The node name, non-empty only for nodes
    name    TEXT,

    -- An array of the outgoing edges; non-empty only for links
    outgoing BIGINT[],

    -- Force the uniqueness of atoms!!
    -- This has the side-effect of creating indexes for fast lookup by
    -- name or by outgoing set.
    UNIQUE (type, name),
    UNIQUE (type, outgoing)
);


-- -----------------------------------------------------------
-- Edge table is not used by the postgres driver.  That is because
-- the array of outgoing edges, above is enough to describe a Link.
-- However, neither MySQL nor SQLite3 support arrays, and so maybe
-- this could be useful for a port to those DB's.
--
-- Table of the edges of the Levi craph corresponding
-- to the hypergraph. An edge is a (src,dst) pair. The
-- pair, understood as going src->dst, for a fixed src,
-- is the set of outgoing edges of the atom. Understood
-- as dst<-src, with fixed dst, are the incoming edges.
--
-- Outgoing edges are understood to be ordered. "pos" is
-- is the order, starting with 0.
--
-- CREATE TABLE Edges (
--  src_uuid  BIGINT,
--  dst_uuid  BIGINT,
--  pos INT
-- );
-- CREATE INDEX inidx ON Edges(src_uuid);
-- CREATE INDEX outidx ON Edges(dst_uuid);

-- -----------------------------------------------------------
-- An SQL table representation for opencog Valuations
--
CREATE TABLE Valuations (
    -- The key for this value
    key BIGINT REFERENCES Atoms(uuid),
    -- The Atom to which this value applies
    atom BIGINT REFERENCES Atoms(uuid),

    -- Value type, e.g. FloatValue, StringValue, etc.
    type  SMALLINT,

    -- An array of values associated with the (key,atom) pair
    -- Only one of the three of float, or string or link should be
    -- non-empty; the other two should be empty. We could have three
    -- different tables, one each for each of these, but then the
    -- UNIQUE constraint is harder to force.
    -- The linkevalue should be an array of vuid's from the Values
    -- table. The "ELEMENT REFERENCES" would do the trick, but it's
    -- not yet implemented in postgres.
    floatvalue DOUBLE PRECISION[],
    stringvalue TEXT[],
    linkvalue BIGINT[], -- ELEMENT REFERENCES Values(vuid),

    UNIQUE (key, atom)
);

-- Index for the fast lookup of all valuations for a given atom.
CREATE INDEX ON Valuations (atom);

-- A recursive overflow table, if recursive values did not directly
-- fit into the Valuation table.
CREATE TABLE Values (
    -- The unique ID for this value
    vuid BIGINT PRIMARY KEY,

    -- Value type, e.g. FloatValue, StringValue, etc.
    type  SMALLINT,

    -- An array of values associated with the vuid.
    -- Only one of the three of float, or string or link should be
    -- non-empty; the other two should be empty.
    -- The linkevalue should be an array of vuid's for other rows
    -- in this table. The "ELEMENT REFERENCES" would do the trick,
    -- but it's not yet implemented in postgres.
    floatvalue DOUBLE PRECISION[],
    stringvalue TEXT[],
    linkvalue BIGINT[] -- ELEMENT REFERENCES Values(vuid)
);

-- -----------------------------------------------------------
-- Table associating type names to stored integer values. The list of
-- type names and numbers may differ from one version of the opencog
-- server to another; thus, we need to convert from type names, to
-- numbers.

CREATE TABLE TypeCodes (
    type SMALLINT UNIQUE,
    typename TEXT UNIQUE
);

-- -----------------------------------------------------------
