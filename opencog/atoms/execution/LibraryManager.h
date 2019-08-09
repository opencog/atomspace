/*
 * opencog/atoms/execution/LibraryManager.h
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

#ifndef _OPENCOG_LIBRARAY_MANAGER_H
#define _OPENCOG_LIBRARAY_MANAGER_H

#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>

class LibraryManager
{
private:
	static std::unordered_map<std::string, void*> _librarys;
	static std::unordered_map<std::string, void*> _functions;
public:
	static void* getFunc(std::string libName,std::string funcName);
	static void setLocalFunc(std::string libName, std::string funcName, void* func);

	/**
	 * Given a grounded schema name like "py: foo", extract
	 * 1. the language, like "py"
	 * 2. the library, like "" if there is none
	 * 3. the function, like "foo"
	 */
	static void parse_schema(const std::string& schema,
	                         std::string& lang,
	                         std::string& lib,
	                         std::string& fun);
};


namespace opencog
{
/**
 * setLocalPredicate("foo", boo) enables creating GroundedPredicateNode
 * with the name "lib:\\foo",  which will call boo on evaluation of
 * corresponding EvaluationLink.
 */
void setLocalPredicate(std::string funcName,
                       TruthValuePtr* (*func)(AtomSpace *, Handle*));

/**
 * setLocalSchema("foo", boo) enables creating GroundedSchemaNode with
 * the name "lib:\\foo", which will call boo on execution of corresponding
 * ExecutionOutputLink.
 */
void setLocalSchema(std::string funcName,
                    Handle* (*func)(AtomSpace *, Handle*));
};
#endif //_OPENCOG_LIBRARAY_MANAGER_H
