/*
 * DynamicDataProxy.cc
 *
 * Copyright (C) 2022 Linas Vepstas
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

#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FormulaStream.h>
#include <opencog/atoms/value/FutureStream.h>
#include <opencog/persist/proxy/DynamicDataProxy.h>

using namespace opencog;

DynamicDataProxy::DynamicDataProxy(const std::string&& name)
	: ProxyNode(DYNAMIC_DATA_PROXY_NODE, std::move(name))
{
	init();
}

DynamicDataProxy::DynamicDataProxy(Type t, const std::string&& name)
	: ProxyNode(t, std::move(name))
{
	init();
}

DynamicDataProxy::~DynamicDataProxy()
{
}

void DynamicDataProxy::init(void)
{
	have_getAtom = true;
	have_loadValue = true;
}

void DynamicDataProxy::getAtom(const Handle& h)
{
	const HandleSet& keys(getKeys());

	for (const Handle& k : keys)
		loadValue(h, k);
}

// We use the Values on *this proxy node itself* as the source
// for ProcedureNodes that will generate dynamic data. Basically,
// we will look for a ProcedureNode on *this proxy*, with the given key.
// If it is found, then wrap it in an ExecutionOutputLink, using
// it as the procedure, and the argument Atom as a argument to the
// procedure. The wrap the whole mess in a Future, so that any access
// to the value results in an evaluation of the ExOutLink.
void DynamicDataProxy::loadValue(const Handle& atom, const Handle& key)
{
	const ValuePtr& rawvp = getValue(key);
	if (nullptr == rawvp) return;

	// Unlikely case: the data is not dynamic.
	if (not rawvp->is_type(PROCEDURE_NODE))
	{
		_atom_space->set_value(atom, key, rawvp);
		return;
	}

	// Ah! Its a procedure! Make it executable!
	Handle exo = _atom_space->add_link(EXECUTION_OUTPUT_LINK,
		HandleCast(rawvp),
		createLink(LIST_LINK, atom));

	// Stick it in a future. Be careful with the type. Anything that
	// is a NumericOutputLink will normally result in a FloatValue;
	// thus a FormulaStream is appropriate. Everything else gets the
	// plainer FutureStream.
	if (rawvp->is_type(NUMERIC_OUTPUT_LINK))
	{
		ValuePtr fut = createFormulaStream(exo);
		_atom_space->set_value(atom, key, fut);
		return;
	}

	ValuePtr fut = createFutureStream(exo);
	_atom_space->set_value(atom, key, fut);
}

DEFINE_NODE_FACTORY(DynamicDataProxy, DYNAMIC_DATA_PROXY_NODE)
