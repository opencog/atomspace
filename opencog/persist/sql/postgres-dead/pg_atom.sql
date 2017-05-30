--
-- Drop all tables first, so you can run this to reset an existing database.
-- Need to drop spaces after Atoms because Atoms references Space table.
DROP TABLE IF EXISTS Atoms;
DROP TABLE IF EXISTS Edges;
DROP TABLE IF EXISTS Spaces;
DROP TABLE IF EXISTS TypeCodes;
DROP TABLE IF EXISTS Global;

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

--
-- Create a table representation for an opencog Atom
--

CREATE TABLE Atoms (
    -- The uuid maps to the atom handle.
    -- Must be unique, and must be non-null. Ergo, primary key.
    -- Use 64-bit int to match the C++ range.
    uuid    BIGINT PRIMARY KEY,

    -- The atomspace that this atom belongs to.
    space BIGINT REFERENCES spaces(space),

    -- Atom type, e.g. Link, Node, etc.
    type  SMALLINT,

    -- maps to TruthValue ID
    -- tvid INT, -- not used, just inline the truth value

    -- Inlined (simple) truth values
    tv_type  SMALLINT,
    stv_mean FLOAT,
    stv_confidence FLOAT,
    stv_count DOUBLE PRECISION,

    -- distance from this link to farthest Node in the outgoing
    -- set of this atom. 
    -- height of Nodes is by definition zero.
    -- height of Links containing only nodes is one, etc.
    height SMALLINT,

    -- The node name, non-empty only for nodes
    name    TEXT,

    out_hash BIGINT,
    out_differentiator SMALLINT,

    -- An array of the outgoing edges; non-empty only for links
    outgoing BIGINT[],

    -- Force the uniqueness of atoms!!
    UNIQUE (type, name),
    UNIQUE (type, out_hash, out_differentiator),
    UNIQUE (type, outgoing)
);

-- Indexes, needed for fast node and link lookup.
-- Make them unique, to catch any errors early.
-- Actually, this is not needed; the unique constraints on the table
-- defacto create indexes; creating them again just doubles the index.
-- CREATE UNIQUE INDEX nodeidx ON Atoms(type, name);
-- CREATE UNIQUE INDEX linkidx ON Atoms(type, outgoing);

-- -----------------------------------------------------------
-- Table of the edges of the Levi graph corresponding 
-- to the hypergraph. An edge is a (src,dst) pair. The
-- pair, understood as going src->dst, for a fixed src,
-- is the set of outgoing edges of the atom. Understood
-- as dst<-src, with fixed dst, are the incoming edges.
--
-- Outgoing edges are understood to be ordered. "pos" is
-- is the order, starting with 0.
--
CREATE TABLE Edges (
    src_uuid  BIGINT,
    dst_uuid  BIGINT,
    pos INT
);
CREATE INDEX src_idx ON Edges(src_uuid, pos);
CREATE INDEX dst_idx ON Edges(dst_uuid);

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
-- Global state

CREATE TABLE Global (
    max_height INT          -- max height of all links.
);

INSERT INTO Global (max_height) VALUES (0); -- largest height observed.
