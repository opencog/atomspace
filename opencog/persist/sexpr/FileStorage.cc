/*
 * FileStorage.cc
 * Read and write Atoms to a file.
 *
 * Copyright (c) 2021 Linas Vepstas <linas@linas.org>
 *
 * LICENSE:
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
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

//#include <error.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include <fstream>

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/storage/storage_types.h>

#include "fast_load.h"
#include "FileStorage.h"
#include "Sexpr.h"

using namespace opencog;

FileStorageNode::FileStorageNode(Type t, const std::string& uri)
	: StorageNode(t, uri)
{
	_fh = nullptr;

	_filename = get_name();

	// If the URL begins with `file://` then just strip that off.
	if (0 == _filename.compare(0, 7, "file://"))
		_filename = _filename.substr(7);
}

FileStorageNode::~FileStorageNode()
{
	if (_fh) fclose(_fh);
	_fh = nullptr;
}

void FileStorageNode::erase(void)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _filename.c_str());

	int rc = ftruncate(fileno(_fh), 0);
	if (rc)
		throw IOException(TRACE_INFO,
		"FileStorageNode cannot erase %s: %s",
			_filename.c_str(), strerror(errno));
}

void FileStorageNode::kill_data(void)
{
	if (_fh) erase();
	else
	{
		int rc = unlink(_filename.c_str());
		if (rc)
			throw IOException(TRACE_INFO,
			"FileStorageNode cannot remove %s: %s",
				_filename.c_str(), strerror(errno));
	}
}

void FileStorageNode::open(void)
{
	if (_fh)
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is already open!", _filename.c_str());
	_fh = fopen(_filename.c_str(), "a+");

	if (nullptr == _fh)
		throw IOException(TRACE_INFO,
		"FileStorageNode cannot open %s: %s",
			_filename.c_str(), strerror(errno));
}

void FileStorageNode::close(void)
{
	if (_fh) fclose(_fh);
	_fh = nullptr;
}

bool FileStorageNode::connected(void)
{
	return nullptr != _fh;
}

void FileStorageNode::barrier(void)
{
	if (_fh) fflush(_fh);
}

Handle FileStorageNode::getNode(Type, const char *)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
	return Handle::UNDEFINED;
}

Handle FileStorageNode::getLink(Type, const HandleSeq&)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
	return Handle::UNDEFINED;
}

void FileStorageNode::getIncomingSet(AtomSpace&, const Handle&)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::getIncomingByType(AtomSpace&, const Handle&, Type t)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::storeAtom(const Handle& h, bool synchronous)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _filename.c_str());

	const std::string sex = Sexpr::dump_atom(h);
	fwrite(sex.c_str(), sex.length(), 1, _fh);
	size_t rc = fwrite("\n", 1, 1, _fh);

	if (1 != rc)
		throw IOException(TRACE_INFO,
		"FileStorageNode failed to store Atom at %s: %s",
			_filename.c_str(), strerror(errno));
}

void FileStorageNode::removeAtom(const Handle&, bool recursive)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::storeValue(const Handle& h, const Handle& key)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _filename.c_str());

	const std::string sex = Sexpr::dump_vatom(h, key);
	fwrite(sex.c_str(), sex.length(), 1, _fh);
	size_t rc = fwrite("\n", 1, 1, _fh);

	if (1 != rc)
		throw IOException(TRACE_INFO,
		"FileStorageNode failed to store Atom at %s: %s",
			_filename.c_str(), strerror(errno));
}

void FileStorageNode::loadValue(const Handle&, const Handle&)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::loadType(AtomSpace&, Type)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::storeAtomSpace(const AtomSpace& table)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _filename.c_str());

	HandleSeq hset;
	table.get_handles_by_type(hset, ATOM, true);
	for(const Handle& h: hset)
	{
		// Store roots, and Atoms that have values.
		// All other Atoms will appear in outgoing sets.
		if (h->haveValues() or 0 == h->getIncomingSetSize())
			storeAtom(h);
	}

	fflush(_fh);
}

void FileStorageNode::loadAtomSpace(AtomSpace& table)
{
	// Check to see if it's connected, and then ignore the file handle.
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _filename.c_str());

	std::ifstream stream(_filename);
	if (not stream.is_open())
		throw IOException(TRACE_INFO,
			"FileStorageNode cannot open %s", _filename.c_str());

	parseStream(stream, table);
	stream.close();
}

DEFINE_NODE_FACTORY(FileStorageNode, FILE_STORAGE_NODE)
