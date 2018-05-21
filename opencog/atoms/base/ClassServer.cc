/*
 * opencog/atoms/base/ClassServer.cc
 *
 * Copyright (C) 2002-2007 Novamente LLC
 * Copyright (C) 2008 by OpenCog Foundation
 * Copyright (C) 2009,2017 Linas Vepstas <linasvepstas@gmail.com>
 * All Rights Reserved
 *
 * Written by Thiago Maia <thiago@vettatech.com>
 *            Andre Senna <senna@vettalabs.com>
 *            Gustavo Gama <gama@vettalabs.com>
 *            Linas Vepstas <linasvepstas@gmail.com>
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

#include "ClassServer.h"

#include <exception>

#include <opencog/atoms/proto/types.h>
#include <opencog/atoms/proto/atom_types.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/proto/ProtoAtom.h>

//#define DPRINTF printf
#define DPRINTF(...)

using namespace opencog;

ClassServer::ClassServer(const NameServer & nameServer):
	_nameServer(nameServer)
{}

static Handle validating_factory(const Handle& atom_to_check)
{
	ClassServer::Validator* checker =
		classserver().getValidator(atom_to_check->get_type());

	/* Well, is it OK, or not? */
	if (not checker(atom_to_check))
		throw SyntaxException(TRACE_INFO,
		     "Invalid Atom syntax: %s",
		     atom_to_check->to_string().c_str());

	return atom_to_check;
}

void ClassServer::spliceFactory(Type t, AtomFactory* fact)
{
	// N.B. it is too late to synchronize calls with NameServer using a shared mutex.
	// If registrations of class names interleave with registration of factories,
	// then the code will not work properly anyway. Before a factory is registered,
	// the complete class hierarchy must be known.
	
	// Find all the factories that belong to parents of this type.
	std::set<AtomFactory*> ok_to_clobber;
	ok_to_clobber.insert(validating_factory);
	for (Type parent=0; parent < t; parent++)
	{
		if (_nameServer.isAncestor(parent, t) and _atomFactory[parent])
			ok_to_clobber.insert(_atomFactory[parent]);
	}

	// Set the factory for all children of this type.  Be careful
	// not to clobber any factories that might have been previously
	// declared.
	for (Type chi=t; chi < _nameServer.getNumberOfClasses(); chi++)
	{
		if (_nameServer.isAncestor(t, chi) and
		    (nullptr == _atomFactory[chi] or
		    ok_to_clobber.end() != ok_to_clobber.find(_atomFactory[chi])))
		{
			_atomFactory[chi] = fact;
		}
	}
}

void ClassServer::addFactory(Type t, AtomFactory* fact)
{
	std::unique_lock<std::mutex> l(factory_mutex);
	_atomFactory.resize(_nameServer.getNumberOfClasses());
	spliceFactory(t, fact);
}

void ClassServer::addValidator(Type t, Validator* checker)
{
	std::unique_lock<std::mutex> l(factory_mutex);
	_validator.resize(_nameServer.getNumberOfClasses());
	_validator[t] = checker;
	spliceFactory(t, validating_factory);

	for (Type chi=t; chi < _nameServer.getNumberOfClasses(); chi++)
	{
		if (_nameServer.isAncestor(t, chi))
			_validator[chi] = checker;
	}
}

ClassServer::AtomFactory* ClassServer::getFactory(Type t) const
{
	return t < _atomFactory.size() ? _atomFactory[t] : nullptr;
}

ClassServer::Validator* ClassServer::getValidator(Type t) const
{
	return t < _validator.size() ? _validator[t] : nullptr;
}

Handle ClassServer::factory(const Handle& h) const
{
	// If there is a factory, then use it.
	AtomFactory* fact = getFactory(h->get_type());
	if (fact)
		return (*fact)(h);

	return h;
}

ClassServer& opencog::classserver()
{
    static std::unique_ptr<ClassServer> instance(new ClassServer(nameserver()));
    return *instance;
}
