/*
 * benchmark/profile_bindlink.cc
 *
 * Copyright (C) 2016 Linas Vepstas
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

#include <iostream>
#include <opencog/guile/load-file.h>
#include <opencog/guile/SchemeEval.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/BindLinkAPI.h>
#include <opencog/util/Config.h>
#include <opencog/util/Logger.h>

using namespace opencog;

AtomSpace *atomspace;
SchemeEval* scheme;

void load_scheme()
{
    // Load some scheme for the setup
    config().set("SCM_PRELOAD",
        "opencog/atoms/base/core_types.scm, "
        "opencog/scm/utilities.scm, "
        "opencog/scm/opencog/query.scm");
    load_scm_files_from_config(*atomspace);
    scheme->eval("(use-modules (opencog))");
    scheme->eval("(use-modules (opencog query))");

    // Define several animals and something of a different type as well
    std::string scheme_animals = 
        "(InheritanceLink (ConceptNode \"Frog\") (ConceptNode \"animal\"))\n"
        "(InheritanceLink (ConceptNode \"Zebra\") (ConceptNode \"animal\"))\n"
        "(InheritanceLink (ConceptNode \"Deer\") (ConceptNode \"animal\"))\n"
        "(InheritanceLink (ConceptNode \"Spaceship\") (ConceptNode \"machine\"))\n";
    scheme->eval(scheme_animals.c_str());
 
}

Handle get_animal_query()
{
    std::string animals_query = "(BindLink \n"
        "  (VariableNode \"$var\")\n"
        "  ;; The pattern to be searched for\n"
        "  (InheritanceLink \n"
        "    (VariableNode \"$var\") \n"
        "    (ConceptNode \"animal\")\n"
        "  )\n"
        "  ;; The value to be returned.\n"
        "  (VariableNode \"$var\")"
        ")";
    return scheme->eval_h(animals_query.c_str());
}

Handle get_animals(Handle& animals_query)
{
    Handle animals;
    for (int index = 0; index < 100000; index++ )
        animals = bindlink(atomspace, animals_query);
    return animals;
}

int main(void)
{

    // Create the atomspace and scheme evaluator.
    atomspace = new AtomSpace();
    scheme = new SchemeEval(atomspace);

    // Load the scheme.
    load_scheme();

    // Get the animal query atom for the bindlink.
    Handle animals_query = get_animal_query();

    // Do the queries.
    Handle animals = get_animals(animals_query);
    LinkPtr link(LinkCast(animals));
    if (link)
    {
        size_t total_animals = link->getOutgoingSet().size();
        std::cout << "total animals = " << total_animals << std::endl;
    }

    return 0;
}