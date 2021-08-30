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

#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/storage/storage_types.h>

#include "FileStorage.h"

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
	printf("hello dtor\n");
}

void FileStorageNode::erase(void)
{
	if (not connected())
		throw IOException(TRACE_INFO,
		"FileStorageNode %s is noty open!", _name.c_str());
}

void FileStorageNode::kill_data(void)
{
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

void FileStorageNode::storeAtom(const Handle&, bool synchronous)
{
	printf("hello storeAtom\n");
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

void FileStorageNode::loadAtomSpace(AtomTable &)
{
	printf("hello loadAtomSpace\n");
}

void FileStorageNode::storeAtomSpace(const AtomTable &)
{
	printf("hello storeAtomSpace\n");
}

DEFINE_NODE_FACTORY(FileStorageNode, FILE_STORAGE_NODE)
