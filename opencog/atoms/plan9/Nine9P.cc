/*
 * opencog/atoms/plan9/Nine9P.cc
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

#include <opencog/atoms/plan9/Nine9P.h>

#include <stdexcept>

namespace opencog
{
namespace plan9
{

// ---------------------------------------------------------------
// Little-endian primitive encode/decode helpers, per the 9P2000
// wire format ("intro(5)": "Numeric parameters are represented in
// one, two, or four bytes... transmitted in least significant to
// most significant order").

static void put8(std::string& s, uint8_t v) { s.push_back((char) v); }

static void put16(std::string& s, uint16_t v)
{
	s.push_back((char) (v & 0xff));
	s.push_back((char) ((v >> 8) & 0xff));
}

static void put32(std::string& s, uint32_t v)
{
	for (int i = 0; i < 4; i++)
		s.push_back((char) ((v >> (8 * i)) & 0xff));
}

static void put64(std::string& s, uint64_t v)
{
	for (int i = 0; i < 8; i++)
		s.push_back((char) ((v >> (8 * i)) & 0xff));
}

static void putstr(std::string& s, const std::string& v)
{
	if (v.size() > 0xffff)
		throw std::runtime_error("9P: string exceeds 65535 bytes");
	put16(s, (uint16_t) v.size());
	s.append(v);
}

static void putqid(std::string& s, const NineQid& q)
{
	put8(s, q.type);
	put32(s, q.version);
	put64(s, q.path);
}

static uint8_t get8(const std::string& s, size_t& p)
{
	if (p + 1 > s.size())
		throw std::runtime_error("9P: truncated message (u8)");
	return (uint8_t) s[p++];
}

static uint16_t get16(const std::string& s, size_t& p)
{
	if (p + 2 > s.size())
		throw std::runtime_error("9P: truncated message (u16)");
	uint16_t v = (uint8_t) s[p] | ((uint16_t) (uint8_t) s[p + 1] << 8);
	p += 2;
	return v;
}

static uint32_t get32(const std::string& s, size_t& p)
{
	if (p + 4 > s.size())
		throw std::runtime_error("9P: truncated message (u32)");
	uint32_t v = 0;
	for (int i = 0; i < 4; i++)
		v |= ((uint32_t) (uint8_t) s[p + i]) << (8 * i);
	p += 4;
	return v;
}

static uint64_t get64(const std::string& s, size_t& p)
{
	if (p + 8 > s.size())
		throw std::runtime_error("9P: truncated message (u64)");
	uint64_t v = 0;
	for (int i = 0; i < 8; i++)
		v |= ((uint64_t) (uint8_t) s[p + i]) << (8 * i);
	p += 8;
	return v;
}

static std::string getstr(const std::string& s, size_t& p)
{
	uint16_t n = get16(s, p);
	if (p + n > s.size())
		throw std::runtime_error("9P: truncated message (string)");
	std::string v = s.substr(p, n);
	p += n;
	return v;
}

static NineQid getqid(const std::string& s, size_t& p)
{
	NineQid q;
	q.type = get8(s, p);
	q.version = get32(s, p);
	q.path = get64(s, p);
	return q;
}

// ---------------------------------------------------------------
// Stat entries (stat(5)).

std::string nine_encode_stat(const NineStat& st)
{
	std::string body;
	put16(body, st.type);
	put32(body, st.dev);
	putqid(body, st.qid);
	put32(body, st.mode);
	put32(body, st.atime);
	put32(body, st.mtime);
	put64(body, st.length);
	putstr(body, st.name);
	putstr(body, st.uid);
	putstr(body, st.gid);
	putstr(body, st.muid);

	// The leading size[2] records the length of everything that
	// follows it (not including itself), per stat(5).
	std::string out;
	put16(out, (uint16_t) body.size());
	out.append(body);
	return out;
}

NineStat nine_decode_stat(const std::string& blob)
{
	size_t p = 0;
	uint16_t size = get16(blob, p);
	if (p + size > blob.size())
		throw std::runtime_error("9P: truncated stat entry");

	NineStat st;
	st.type = get16(blob, p);
	st.dev = get32(blob, p);
	st.qid = getqid(blob, p);
	st.mode = get32(blob, p);
	st.atime = get32(blob, p);
	st.mtime = get32(blob, p);
	st.length = get64(blob, p);
	st.name = getstr(blob, p);
	st.uid = getstr(blob, p);
	st.gid = getstr(blob, p);
	st.muid = getstr(blob, p);
	return st;
}

// ---------------------------------------------------------------
// Message framing.

std::string nine_encode(const NineMsg& m)
{
	std::string body;
	put8(body, m.type);
	put16(body, m.tag);

	switch (m.type)
	{
		case Tversion:
		case Rversion:
			put32(body, m.msize);
			putstr(body, m.version);
			break;

		case Tauth:
			put32(body, m.afid);
			putstr(body, m.uname);
			putstr(body, m.aname);
			break;

		case Rauth:
			putqid(body, m.qid);
			break;

		case Tattach:
			put32(body, m.fid);
			put32(body, m.afid);
			putstr(body, m.uname);
			putstr(body, m.aname);
			break;

		case Rattach:
			putqid(body, m.qid);
			break;

		case Rerror:
			putstr(body, m.ename);
			break;

		case Tflush:
			put16(body, m.oldtag);
			break;

		case Rflush:
			break;

		case Twalk:
			put32(body, m.fid);
			put32(body, m.newfid);
			put16(body, (uint16_t) m.wname.size());
			for (const auto& n : m.wname)
				putstr(body, n);
			break;

		case Rwalk:
			put16(body, (uint16_t) m.wqid.size());
			for (const auto& q : m.wqid)
				putqid(body, q);
			break;

		case Topen:
			put32(body, m.fid);
			put8(body, m.mode);
			break;

		case Ropen:
			putqid(body, m.qid);
			put32(body, m.iounit);
			break;

		case Tcreate:
			put32(body, m.fid);
			putstr(body, m.name);
			put32(body, m.perm);
			put8(body, m.mode);
			break;

		case Rcreate:
			putqid(body, m.qid);
			put32(body, m.iounit);
			break;

		case Tread:
			put32(body, m.fid);
			put64(body, m.offset);
			put32(body, m.count);
			break;

		case Rread:
			put32(body, (uint32_t) m.data.size());
			body.append(m.data);
			break;

		case Twrite:
			put32(body, m.fid);
			put64(body, m.offset);
			put32(body, (uint32_t) m.data.size());
			body.append(m.data);
			break;

		case Rwrite:
			put32(body, m.count);
			break;

		case Tclunk:
		case Tremove:
		case Tstat:
			put32(body, m.fid);
			break;

		case Rclunk:
		case Rremove:
		case Rwstat:
			break;

		case Rstat:
			putstr(body, m.stat);
			break;

		case Twstat:
			put32(body, m.fid);
			putstr(body, m.stat);
			break;

		default:
			throw std::runtime_error("9P: cannot encode unknown message type");
	}

	std::string out;
	put32(out, (uint32_t) (4 + body.size()));
	out.append(body);
	return out;
}

size_t nine_decode(const std::string& bytes, NineMsg& m)
{
	if (bytes.size() < 4)
		return 0;

	size_t hp = 0;
	uint32_t size = get32(bytes, hp);
	if (size < 7)
		throw std::runtime_error("9P: message shorter than minimum header");
	if (bytes.size() < size)
		return 0; // Not enough bytes buffered yet; try again later.

	// Decode strictly within this message's own declared length, so
	// that any additional bytes already queued behind it (the start
	// of a subsequent message) can never be misread as part of it.
	const std::string frame = bytes.substr(0, size);

	m = NineMsg();
	size_t p = 4;
	m.type = get8(frame, p);
	m.tag = get16(frame, p);

	switch (m.type)
	{
		case Tversion:
		case Rversion:
			m.msize = get32(frame, p);
			m.version = getstr(frame, p);
			break;

		case Tauth:
			m.afid = get32(frame, p);
			m.uname = getstr(frame, p);
			m.aname = getstr(frame, p);
			break;

		case Rauth:
			m.qid = getqid(frame, p);
			break;

		case Tattach:
			m.fid = get32(frame, p);
			m.afid = get32(frame, p);
			m.uname = getstr(frame, p);
			m.aname = getstr(frame, p);
			break;

		case Rattach:
			m.qid = getqid(frame, p);
			break;

		case Rerror:
			m.ename = getstr(frame, p);
			break;

		case Tflush:
			m.oldtag = get16(frame, p);
			break;

		case Rflush:
			break;

		case Twalk:
		{
			m.fid = get32(frame, p);
			m.newfid = get32(frame, p);
			uint16_t n = get16(frame, p);
			for (uint16_t i = 0; i < n; i++)
				m.wname.push_back(getstr(frame, p));
			break;
		}

		case Rwalk:
		{
			uint16_t n = get16(frame, p);
			for (uint16_t i = 0; i < n; i++)
				m.wqid.push_back(getqid(frame, p));
			break;
		}

		case Topen:
			m.fid = get32(frame, p);
			m.mode = get8(frame, p);
			break;

		case Ropen:
			m.qid = getqid(frame, p);
			m.iounit = get32(frame, p);
			break;

		case Tcreate:
			m.fid = get32(frame, p);
			m.name = getstr(frame, p);
			m.perm = get32(frame, p);
			m.mode = get8(frame, p);
			break;

		case Rcreate:
			m.qid = getqid(frame, p);
			m.iounit = get32(frame, p);
			break;

		case Tread:
			m.fid = get32(frame, p);
			m.offset = get64(frame, p);
			m.count = get32(frame, p);
			break;

		case Rread:
		{
			uint32_t n = get32(frame, p);
			if (p + n > frame.size())
				throw std::runtime_error("9P: truncated Rread data");
			m.data = frame.substr(p, n);
			p += n;
			break;
		}

		case Twrite:
		{
			m.fid = get32(frame, p);
			m.offset = get64(frame, p);
			uint32_t n = get32(frame, p);
			if (p + n > frame.size())
				throw std::runtime_error("9P: truncated Twrite data");
			m.data = frame.substr(p, n);
			p += n;
			break;
		}

		case Rwrite:
			m.count = get32(frame, p);
			break;

		case Tclunk:
		case Tremove:
		case Tstat:
			m.fid = get32(frame, p);
			break;

		case Rclunk:
		case Rremove:
		case Rwstat:
			break;

		case Rstat:
			m.stat = getstr(frame, p);
			break;

		case Twstat:
			m.fid = get32(frame, p);
			m.stat = getstr(frame, p);
			break;

		default:
			throw std::runtime_error("9P: unknown message type");
	}

	return size;
}

NineMsg nine_error(uint16_t tag, const std::string& ename)
{
	NineMsg m;
	m.type = Rerror;
	m.tag = tag;
	m.ename = ename;
	return m;
}

} // namespace plan9
} // namespace opencog
