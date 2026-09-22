/*
 * opencog/atoms/plan9/Nine9P.h
 *
 * Minimal, from-scratch encoder/decoder for the Plan 9 "9P2000"
 * filesystem namespace protocol, as documented in the Plan 9
 * manual pages intro(5) and 9p(2)/version(5).
 *
 * Only the wire format is implemented here (message framing,
 * Qids, and Stat entries). Dispatch against an AtomSpace-backed
 * synthetic filesystem is implemented separately, in NineServer.h.
 *
 * Copyright (c) 2024 OpenCog Foundation
 * All Rights Reserved
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _OPENCOG_NINE_9P_H
#define _OPENCOG_NINE_9P_H

#include <cstdint>
#include <string>
#include <vector>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

/// 9P2000 message type codes (intro(5)).
enum NineMsgType : uint8_t
{
	Tversion = 100, Rversion = 101,
	Tauth    = 102, Rauth    = 103,
	Tattach  = 104, Rattach  = 105,
	Rerror   = 107,
	Tflush   = 108, Rflush   = 109,
	Twalk    = 110, Rwalk    = 111,
	Topen    = 112, Ropen    = 113,
	Tcreate  = 114, Rcreate  = 115,
	Tread    = 116, Rread    = 117,
	Twrite   = 118, Rwrite   = 119,
	Tclunk   = 120, Rclunk   = 121,
	Tremove  = 122, Rremove  = 123,
	Tstat    = 124, Rstat    = 125,
	Twstat   = 126, Rwstat   = 127,
};

/// Qid.type bits (stat(5)).
enum NineQidType : uint8_t
{
	QTDIR    = 0x80,
	QTAPPEND = 0x40,
	QTEXCL   = 0x20,
	QTAUTH   = 0x08,
	QTTMP    = 0x04,
	QTFILE   = 0x00,
};

/// Open/create mode bits (open(5)), subset actually used here.
enum NineOpenMode : uint8_t
{
	OREAD  = 0,
	OWRITE = 1,
	ORDWR  = 2,
	OTRUNC = 0x10,
};

/// Directory permission bit, as used in Stat.mode and Tcreate.perm.
static const uint32_t NINE_DMDIR = 0x80000000u;

static const uint32_t NINE_NOFID = 0xFFFFFFFFu;
static const uint16_t NINE_NOTAG = 0xFFFFu;
static const uint32_t NINE_DEFAULT_MSIZE = 8192;
static const char* const NINE_VERSION = "9P2000";

/// A 13-byte Qid: server-unique file identity plus type/version.
struct NineQid
{
	uint8_t  type = QTFILE;
	uint32_t version = 0;
	uint64_t path = 0;

	bool operator==(const NineQid& o) const
	{
		return type == o.type and version == o.version and path == o.path;
	}
	bool operator!=(const NineQid& o) const { return not (*this == o); }
};

/// A decoded stat(5) directory-entry structure.
struct NineStat
{
	uint16_t type = 0;
	uint32_t dev = 0;
	NineQid qid;
	uint32_t mode = 0;
	uint32_t atime = 0;
	uint32_t mtime = 0;
	uint64_t length = 0;
	std::string name;
	std::string uid;
	std::string gid;
	std::string muid;
};

/**
 * A decoded 9P2000 message. Only the fields relevant to `type` are
 * meaningful; this is a flat struct (not a strict union) for
 * simplicity, since 9P messages are small and this is not a
 * performance-critical hot path.
 */
struct NineMsg
{
	uint8_t  type = 0;
	uint16_t tag = NINE_NOTAG;

	uint32_t fid = 0;
	uint32_t afid = 0;
	uint32_t newfid = 0;
	std::string uname;
	std::string aname;
	std::string ename;                    // Rerror
	std::string version = NINE_VERSION;   // T/Rversion
	uint32_t msize = NINE_DEFAULT_MSIZE;
	NineQid qid;                          // single-qid replies
	std::vector<std::string> wname;       // Twalk
	std::vector<NineQid> wqid;             // Rwalk
	uint8_t mode = 0;                     // Topen/Tcreate
	uint32_t perm = 0;                    // Tcreate
	std::string name;                     // Tcreate
	uint64_t offset = 0;                  // Tread/Twrite
	uint32_t count = 0;                   // Tread request / Rwrite reply
	std::string data;                     // Rread payload / Twrite payload
	uint16_t oldtag = NINE_NOTAG;          // Tflush
	std::string stat;                     // encoded stat[] blob
	uint32_t iounit = 0;                  // Ropen/Rcreate
};

/// Encode `msg` (dispatching on msg.type) as a 9P2000 wire message,
/// including its leading size[4] field. Throws std::runtime_error
/// if `msg.type` is not a message type this encoder understands.
std::string nine_encode(const NineMsg& msg);

/**
 * Decode exactly one 9P2000 message from the front of `bytes`.
 * Returns the number of bytes consumed (the message's own size[4]
 * field), or 0 if `bytes` does not yet hold a complete message.
 * Throws std::runtime_error if the bytes present are malformed.
 */
size_t nine_decode(const std::string& bytes, NineMsg& out);

/// Build an Rerror reply for `tag` with the given error text.
NineMsg nine_error(uint16_t tag, const std::string& ename);

/// Encode a NineStat as the byte-counted stat[] blob used inside
/// Rstat/Twstat (and, doubly length-prefixed, inside a directory's
/// Rread payload).
std::string nine_encode_stat(const NineStat& st);

/// Decode a stat[] blob produced by nine_encode_stat().
NineStat nine_decode_stat(const std::string& blob);

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // _OPENCOG_NINE_9P_H
