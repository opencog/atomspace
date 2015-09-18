/*
 * ForwardChainer.cc
 *
 * Copyright (C) 2014,2015 Misgana Bayetta
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>
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

#include <opencog/util/Logger.h>
#include <opencog/atoms/bind/PatternLink.h>
#include <opencog/atomutils/AtomUtils.h>
#include <opencog/query/DefaultImplicator.h>
#include <opencog/rule-engine/Rule.h>
#include <opencog/atoms/bind/BindLink.h>
#include "ForwardChainer.h"
#include "ForwardChainerCallBack.h"

using namespace opencog;

ForwardChainer::ForwardChainer(AtomSpace& as, Handle rbs) :
	_as(as), _rec(_as), _rbs(rbs), _configReader(as, rbs), _fcmem(&as)
{
    init();
}

void ForwardChainer::init()
{
    _fcmem.set_search_in_af(_configReader.get_attention_allocation());
    _fcmem.set_rules(_configReader.get_rules());
    _fcmem.set_cur_rule(nullptr);

    // Provide a logger
    _log = NULL;
    setLogger(new opencog::Logger("forward_chainer.log", Logger::FINE, true));
}

void ForwardChainer::setLogger(Logger* log)
{
    if (_log)
        delete _log;
    _log = log;
}

Logger* ForwardChainer::getLogger()
{
    return _log;
}

/**
 * Does one step forward chaining
 *
 * @param fcb a concrete implementation of of ForwardChainerCallBack class 
 */
UnorderedHandleSet ForwardChainer::do_step(bool search_focus_set/* = false*/)
{

    Handle hsource = choose_next_source(_fcmem);

    _log->debug("[ForwardChainer] Next source %s", hsource->toString().c_str());

    _fcmem.set_source(hsource);

    HandleSeq derived_rhandles;

    //choose a rule that source unifies with one of its premises.
    Rule *rule = choose_rule(hsource, false);
    if (rule) {
        _fcmem.set_cur_rule(rule);
        derived_rhandles = derive_rules(hsource, rule);

    } else {
        //choose rule that unifies that source unifies with sub-atoms of its premises.
        rule = choose_rule(hsource, true);

        if (rule) {
            _fcmem.set_cur_rule(rule);
            derived_rhandles = derive_rules(hsource, rule,
            true);
        }
    }

    _log->debug( "Derived rule size = %d", derived_rhandles.size());

    UnorderedHandleSet products;

    for (Handle rhandle : derived_rhandles) {
        HandleSeq temp_result = apply_rule(rhandle,search_focus_set);

        std::copy(temp_result.begin(), temp_result.end(),
                  std::inserter(products, products.end()));
    }

    return products;
}

void ForwardChainer::do_chain(Handle hsource, HandleSeq focus_set)
{

    validate(hsource,focus_set);

    _fcmem.set_focus_set(focus_set);

    HandleSeq init_sources;
    //Accept set of initial sources wrapped in a SET_LINK
    if(LinkCast(hsource) and hsource->getType() == SET_LINK)
    {
     init_sources = _as.get_outgoing(hsource);

     //Relex2Logic uses this.TODO make a separate class
     //to handle this robustly.
     if(init_sources.empty())
     {
         bool search_in_af = not focus_set.empty();
         apply_all_rules(search_in_af);
         return;
     }

    }
    else
    {
        init_sources.push_back(hsource);
    }

    // Variable fulfillment query.
    UnorderedHandleSet var_nodes = get_outgoing_nodes(hsource,
                                                      { VARIABLE_NODE });
    if (not var_nodes.empty())
        return do_pm(hsource, var_nodes);

    // Default forward chaining
    _fcmem.update_potential_sources(init_sources);

    auto max_iter = _configReader.get_maximum_iterations();

    while (_iteration < max_iter /*OR other termination criteria*/) {

        _log->debug("Iteration %d", _iteration);

        UnorderedHandleSet products;

        if (focus_set.empty())
            products = do_step(false);
        else
            products = do_step(true);

        _fcmem.add_rules_product(_iteration,
                                 HandleSeq(products.begin(), products.end()));
        _fcmem.update_potential_sources(
                HandleSeq(products.begin(), products.end()));

        _iteration++;
    }

    _log->debug("[ForwardChainer] finished forwarch chaining.");
}

/**
 * Does pattern matching for a variable containing query.
 * @param source a variable containing handle passed as an input to the pattern matcher
 * @param var_nodes the VariableNodes in @param hsource
 * @param fcb a forward chainer callback implementation used here only for choosing rules
 * that contain @param hsource in their implicant
 */
void ForwardChainer::do_pm(const Handle& hsource,
                           const UnorderedHandleSet& var_nodes,
                           ForwardChainerCallBack& fcb)
{
    DefaultImplicator impl(&_as);
    impl.implicand = hsource;
    HandleSeq vars;
    for (auto h : var_nodes)
        vars.push_back(h);
    _fcmem.set_source(hsource);
    Handle hvar_list = _as.add_link(VARIABLE_LIST, vars);
    Handle hclause = _as.add_link(AND_LINK, hsource);

    // Run the pattern matcher, find all patterns that satisfy the
    // the clause, with the given variables in it.
    PatternLinkPtr sl(createPatternLink(hvar_list, hclause));
    sl->satisfy(impl);

    // Update result
    _fcmem.add_rules_product(0, impl.get_result_list());

    // Delete the AND_LINK and LIST_LINK
    _as.remove_atom(hvar_list);
    _as.remove_atom(hclause);

    //!Additionally, find applicable rules and apply.
    vector<Rule*> rules = fcb.choose_rules(_fcmem);
    for (Rule* rule : rules) {
        BindLinkPtr bl(BindLinkCast(rule->get_handle()));
        DefaultImplicator impl(&_as);
        impl.implicand = bl->get_implicand();
        bl->imply(impl);
        _fcmem.set_cur_rule(rule);
        _fcmem.add_rules_product(0, impl.get_result_list());
    }
}
/**
 * Invokes pattern matcher using each rule declared in the configuration file.
 */
void ForwardChainer::do_pm()
{
    //! Do pattern matching using the rules declared in the declaration file
    _log->info("Forward chaining on the rule-based system %s "
               "declared in %s", _rbs->toString().c_str());
    vector<Rule*> rules = _fcmem.get_rules();
    for (Rule* rule : rules) {
        _log->info("Applying rule %s on ", rule->get_name().c_str());
        BindLinkPtr bl(BindLinkCast(rule->get_handle()));
        DefaultImplicator impl(&_as);
        impl.implicand = bl->get_implicand();
        bl->imply(impl);
        _fcmem.set_cur_rule(rule);

        _log->info("OUTPUTS");
        for (auto h : impl.get_result_list())
            _log->info("%s", h->toString().c_str());

        _fcmem.add_rules_product(0, impl.get_result_list());
    }

}

HandleSeq ForwardChainer::get_chaining_result()
{
    return _fcmem.get_result();
}
