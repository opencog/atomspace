/*
 * opencog/atoms/plan9/NineTcpServer.h
 *
 * An optional, minimal TCP transport for NineServer, so that the
 * AtomSpace-backed 9P filesystem can actually be mounted by a real
 * Plan 9 style namespace client -- for example Linux's v9fs
 * ("mount -t 9p -o port=<port>,version=9p2000 <host> /mnt/atomspace"),
 * 9pfuse, or a Plan 9 (9front) kernel's own mount(1)/import(4).
 *
 * This is intentionally a small, blocking, one-thread-per-connection
 * listener using plain BSD sockets -- no new external dependency
 * (such as boost::asio) is introduced. It is POSIX-only, matching
 * the rest of AtomSpace's primary target platform (GNU/Linux); it is
 * compiled out entirely on Windows.
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

#ifndef _OPENCOG_NINE_TCP_SERVER_H
#define _OPENCOG_NINE_TCP_SERVER_H

#ifndef _WIN32

#include <atomic>
#include <cstdint>
#include <thread>
#include <vector>

#include <opencog/atoms/plan9/NineNamespace.h>

namespace opencog
{
namespace plan9
{
/** \addtogroup grp_atomspace
 *  @{
 */

/**
 * Listens on a TCP port and serves 9P2000 to each connecting client,
 * using a fresh NineServer (and therefore a fresh, independent
 * per-process NineNamespace copy) per connection.
 */
class NineTcpServer
{
public:
	/// `ns` is the template namespace each new connection's private
	/// NineNamespace is copy-constructed from.
	explicit NineTcpServer(NineNamespace ns);
	~NineTcpServer();

	/// Start listening on `port` (0 = pick an ephemeral port) and
	/// accepting connections on a background thread. Returns the
	/// actual bound port.
	uint16_t start(uint16_t port = 0);

	/// Stop accepting new connections and join the accept thread.
	/// Already-open connections are closed.
	void stop();

	bool running() const { return _running.load(); }

private:
	void accept_loop();
	static void serve_client(int fd, NineNamespace ns);

	NineNamespace _template_ns;
	int _listen_fd = -1;
	uint16_t _port = 0;
	std::atomic<bool> _running{false};
	std::thread _accept_thread;
};

/** @}*/
} // namespace plan9
} // namespace opencog

#endif // !_WIN32
#endif // _OPENCOG_NINE_TCP_SERVER_H
