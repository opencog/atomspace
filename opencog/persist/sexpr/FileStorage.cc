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

#include <error.h>
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
	// Nothing to do. Never fail.
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
		"FileStorageNode %s is not open!", _name.c_str());

	ftruncate(fileno(_fh), 0);
}

void FileStorageNode::kill_data(void)
{
	if (_fh) erase();
	else unlink(_name.c_str());
}

void FileStorageNode::open(void)
{
	if (_fh)
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is already open!", _name.c_str());
	_fh = fopen(_name.c_str(), "a+");

	if (nullptr == _fh)
		throw IOException(TRACE_INFO,
		"FileStorageNode cannot open %s: %s",
			_name.c_str(), strerror(errno));
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

void FileStorageNode::getIncomingSet(AtomTable&, const Handle&)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::getIncomingByType(AtomTable&, const Handle&, Type t)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::storeAtom(const Handle& h, bool synchronous)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _name.c_str());

	const std::string sex = Sexpr::dump_atom(h);
	fwrite(sex.c_str(), sex.length(), 1, _fh);
	fwrite("\n", 1, 1, _fh);
}

void FileStorageNode::removeAtom(const Handle&, bool recursive)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::storeValue(const Handle&, const Handle&)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::loadValue(const Handle&, const Handle&)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::loadType(AtomTable&, Type)
{
	throw IOException(TRACE_INFO,
		"FileStorageNode does not support this operation!");
}

void FileStorageNode::storeAtomSpace(const AtomTable& table)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _name.c_str());

	HandleSet hset;
	table.getRootSetByType(hset, ATOM, true);
	for(const Handle& h: hset)
		storeAtom(h);

	fflush(_fh);
}

void FileStorageNode::loadAtomSpace(AtomTable& table)
{
printf("hello loadAtomSpace\n");
	// Check to see if it's connected, and then ignore the file handle.
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is not open!", _name.c_str());

	std::ifstream stream(_name);
	if (not stream.is_open())
		throw IOException(TRACE_INFO,
			"FileStorageNode cannot open %s", _name.c_str());

	parseStream(stream, *table.getAtomSpace());
	stream.close();
}

DEFINE_NODE_FACTORY(FileStorageNode, FILE_STORAGE_NODE)
