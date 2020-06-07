/*
 * opencog/atoms/execution/LibraryManager.cc
 *
 * Copyright (C) 2018 OpenCog Foundation
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

#include <dlfcn.h>

#include "LibraryManager.h"

using namespace opencog;

std::unordered_map<std::string, void*> LibraryManager::_librarys;
std::unordered_map<std::string, void*> LibraryManager::_functions;

void LibraryManager::setLocalFunc(std::string libName, std::string funcName, void* func)
{
	if (_librarys.count(libName) == 0) {
		_librarys[libName] = NULL;
	}
	std::string funcID = libName + "\\" + funcName;
	_functions[funcID] = func;
}

void* LibraryManager::getFunc(std::string libName, std::string funcName)
{
	void* libHandle;
	if (_librarys.count(libName) == 0) {
		// Try and load the library and function.
		libHandle = dlopen(libName.c_str(), RTLD_LAZY);
		if (nullptr == libHandle)
			throw RuntimeException(TRACE_INFO,
			                       "Cannot open library: %s - %s", libName.c_str(), dlerror());
		_librarys[libName] = libHandle;
	}
	else {
		libHandle = _librarys[libName];
	}

	std::string funcID = libName + "\\" + funcName;

	void* sym;
	if (_functions.count(funcID) == 0){
		sym = dlsym(libHandle, funcName.c_str());
		if (nullptr == sym)
			throw RuntimeException(TRACE_INFO,
			                       "Cannot find symbol %s in library: %s - %s",
			                       funcName.c_str(), libName.c_str(), dlerror());
		_functions[funcID] = sym;
	}
	else {
		sym = _functions[funcID];
	}

	return sym;
}

void LibraryManager::parse_schema(const std::string& schema,
                                  std::string& lang,
                                  std::string& lib,
                                  std::string& fun)
{
	std::string::size_type pos = schema.find(":");
	if (pos == std::string::npos)
		return;

	lang = schema.substr(0, pos);

	// Move past the colon and strip leading white-space
	do { pos++; } while (' ' == schema[pos]);

	if (lang == "lib") {
		// Get the name of the Library and Function. They should be
		// separated by '\'. If no library the separator may be omitted.
		std::size_t seppos = schema.find("\\");
		if (seppos == std::string::npos) { // No library
			lib = "";
			fun = schema.substr(pos);
		} else {                  // Possible library
			lib = schema.substr(pos, seppos - pos);
			fun = schema.substr(seppos + 1);
		}
	} else
		fun = schema.substr(pos);
}

void opencog::setLocalSchema(std::string funcName,
                             Handle* (*func)(AtomSpace *, Handle*))
{
	LibraryManager::setLocalFunc("", funcName, reinterpret_cast<void*>(func));
}

void opencog::setLocalPredicate(std::string funcName,
                                TruthValuePtr* (*func)(AtomSpace *, Handle*))
{
   LibraryManager::setLocalFunc("", funcName, reinterpret_cast<void*>(func));
}
