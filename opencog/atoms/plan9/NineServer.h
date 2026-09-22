/*
 * opencog/atoms/plan9/NineServer.h
 *
 * Drives a NineNamespace from decoded 9P2000 messages (see Nine9P.h),
 * implementing the fid lifecycle (attach/walk/open/read/write/clunk)
 * of a Plan 9 file server. This class is transport-agnostic: pair it
 * with any byte stream (TCP, a Unix socket, an in-process pipe, or --
 * as in the unit tests -- nothing at all, by calling dispatch() on
 * already-decoded messages directly).
 *
 * Each NineServer instance is exactly one Plan 9 "process": it owns
 * one private NineNamespace (copy-constructed from whatever template
 * namespace the caller provides), so binds/mounts performed through
 * one connection are invisible to any other.
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

#ifndef _OPENCOG_NINE_SERVER_H
#define _OPENCOG_NINE_SERVER_H

#include <string>
#include <unordered_map>

#include <opencog/atoms/plan9/Nine9P.h>
#include <opencog/atoms/plan9/NineNamespace.h>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

struct NineFid
{
	std::string path;
	std::vector<NineNodePtr> stack;
	uint8_t mode = 0;
	bool opened = false;
};

class NineServer
{
public:
	explicit NineServer(const NineNamespace& ns);

	/// Process one already-decoded 9P message and produce the reply.
	NineMsg dispatch(const NineMsg& req);

	/// Decode exactly one message from the front of `request`,
	/// dispatch it, and return the encoded reply. Throws
	/// std::runtime_error if `request` does not hold a complete
	/// message (callers streaming from a socket should buffer until
	/// nine_decode() would succeed before calling this).
	std::string handle_message(const std::string& request);

	NineNamespace& namespace_table() { return _ns; }
	const NineNamespace& namespace_table() const { return _ns; }

private:
	NineNamespace _ns;
	std::unordered_map<uint32_t, NineFid> _fids;
	uint32_t _msize = NINE_DEFAULT_MSIZE;

	NineMsg do_version(const NineMsg&);
	NineMsg do_attach(const NineMsg&);
	NineMsg do_walk(const NineMsg&);
	NineMsg do_open(const NineMsg&);
	NineMsg do_read(const NineMsg&);
	NineMsg do_write(const NineMsg&);
	NineMsg do_clunk(const NineMsg&);
	NineMsg do_remove(const NineMsg&);
	NineMsg do_stat(const NineMsg&);
	NineMsg do_flush(const NineMsg&);

	static NineQid top_qid(const std::vector<NineNodePtr>& stack);
	static bool stack_is_dir(const std::vector<NineNodePtr>& stack);
	static std::string parent_path(const std::string& path);
	static std::string join_path(const std::string& path, const std::string& c);
	NineStat stat_for(const std::string& path,
	                   const std::vector<NineNodePtr>& stack) const;
};

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // _OPENCOG_NINE_SERVER_H
