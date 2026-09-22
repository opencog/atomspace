/*
 * opencog/atoms/plan9/NineTcpServer.cc
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

#ifndef _WIN32

#include <opencog/atoms/plan9/NineTcpServer.h>
#include <opencog/atoms/plan9/NineServer.h>

#include <cstring>
#include <stdexcept>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

namespace opencog
{
namespace plan9
{

NineTcpServer::NineTcpServer(NineNamespace ns) : _template_ns(std::move(ns))
{}

NineTcpServer::~NineTcpServer()
{
	stop();
}

uint16_t NineTcpServer::start(uint16_t port)
{
	_listen_fd = ::socket(AF_INET, SOCK_STREAM, 0);
	if (_listen_fd < 0)
		throw std::runtime_error("NineTcpServer: socket() failed");

	int one = 1;
	::setsockopt(_listen_fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

	struct sockaddr_in addr;
	std::memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	addr.sin_port = htons(port);

	if (0 != ::bind(_listen_fd, (struct sockaddr*) &addr, sizeof(addr)))
	{
		::close(_listen_fd);
		_listen_fd = -1;
		throw std::runtime_error("NineTcpServer: bind() failed");
	}

	socklen_t alen = sizeof(addr);
	if (0 == ::getsockname(_listen_fd, (struct sockaddr*) &addr, &alen))
		_port = ntohs(addr.sin_port);

	if (0 != ::listen(_listen_fd, 8))
	{
		::close(_listen_fd);
		_listen_fd = -1;
		throw std::runtime_error("NineTcpServer: listen() failed");
	}

	_running.store(true);
	_accept_thread = std::thread(&NineTcpServer::accept_loop, this);
	return _port;
}

void NineTcpServer::stop()
{
	if (not _running.exchange(false))
		return;
	if (_listen_fd >= 0)
	{
		::shutdown(_listen_fd, SHUT_RDWR);
		::close(_listen_fd);
		_listen_fd = -1;
	}
	if (_accept_thread.joinable())
		_accept_thread.join();
}

void NineTcpServer::accept_loop()
{
	while (_running.load())
	{
		int fd = ::accept(_listen_fd, nullptr, nullptr);
		if (fd < 0)
			break; // Listening socket was closed by stop().

		std::thread(&NineTcpServer::serve_client, fd, _template_ns).detach();
	}
}

void NineTcpServer::serve_client(int fd, NineNamespace ns)
{
	NineServer server(ns);
	std::string buf;
	char chunk[4096];

	while (true)
	{
		// Try to decode as many complete messages as are already
		// buffered before asking for more bytes.
		while (true)
		{
			NineMsg req;
			size_t used;
			try
			{
				used = nine_decode(buf, req);
			}
			catch (const std::exception&)
			{
				::close(fd);
				return; // Malformed stream; drop the connection.
			}
			if (0 == used)
				break; // Need more bytes.

			NineMsg rep = server.dispatch(req);
			std::string wire = nine_encode(rep);
			ssize_t sent = 0;
			while ((size_t) sent < wire.size())
			{
				ssize_t n = ::send(fd, wire.data() + sent,
					wire.size() - sent, 0);
				if (n <= 0) { ::close(fd); return; }
				sent += n;
			}
			buf.erase(0, used);
		}

		ssize_t n = ::recv(fd, chunk, sizeof(chunk), 0);
		if (n <= 0) break; // Peer closed, or error.
		buf.append(chunk, n);
	}
	::close(fd);
}

} // namespace plan9
} // namespace opencog

#endif // !_WIN32
