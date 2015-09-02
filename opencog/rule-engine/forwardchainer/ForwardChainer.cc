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
 *
 * @return An unordered sets of result of applying a particular selected rule.
 */
UnorderedHandleSet ForwardChainer::do_step(ForwardChainerCallBack& fcb,
                                           bool search_focus_set/* = false*/)
{

    if (_fcmem.get_cur_source() == Handle::UNDEFINED) {
        _log->info("[ForwardChainer] No current source, step "
                   "forward chaining aborted.");
        return {};
    }

    _log->info("[ForwardChainer] Next source %s",
               _fcmem.get_cur_source()->toString().c_str());

    _log->info("[ForwardChainer] Selecting a rule from the set of "
               "candidate rules.");

    Rule* r = _rec.tournament_select(rule_weight);
    _log->info("[ForwardChainer] Selected rule is %s",
               (r->get_handle())->toShortString().c_str());

    _fcmem.set_cur_rule(r);
    //TODO Should we use all derived rules or what?
    UnorderedHandleSet products;

    HandleSeq derived_rhandles = fcb.derive_rules(_fcmem.get_cur_source(), r);

    //try sub-atom unification on failure to full unification
    if (derived_rhandles.empty())
        derived_rhandles = fcb.derive_rules(_fcmem.get_cur_source(), r, true);

    HandleSeq temp_result;
    if (not _fcmem.get_focus_set().empty()) {

        for (Handle rhandle : derived_rhandles) {
            temp_result = fcb.apply_rule(rhandle, true);
            std::copy(temp_result.begin(), temp_result.end(),
                      std::inserter(products, products.end()));
        }

    } else {

        for (Handle rhandle : derived_rhandles) {
            temp_result = fcb.apply_rule(rhandle);
            std::copy(temp_result.begin(), temp_result.end(),
                      std::inserter(products, products.end()));
        }
    }

    //done with this rule.
    rule_weight.erase(r);

    return products;
}

void ForwardChainer::do_chain(ForwardChainerCallBack& fcb,
                              Handle hsource/*=Handle::UNDEFINED*/,
                              HandleSeq focus_set /*= {}*/,
                              bool search_focus_set/* = false*/)
{
    if (hsource == Handle::UNDEFINED) {
        do_pm();
        return;
    }

   _fcmem.set_focus_set(focus_set);

    fcb._fcmem = &_fcmem;

    // Variable fulfillment query.
    UnorderedHandleSet var_nodes = get_outgoing_nodes(hsource,
                                                      { VARIABLE_NODE });
    if (not var_nodes.empty())
        return do_pm(hsource, var_nodes, fcb);

    HandleSeq init_sources;
    //Accept set of initial sources wrapped by a SET_LINK
    if(LinkCast(hsource) and hsource->getType() == SET_LINK)
     init_sources = _as.get_outgoing(hsource);
    else
        init_sources.push_back(hsource);

    _fcmem.update_potential_sources(init_sources);

    auto max_iter = _configReader.get_maximum_iterations();

    while (_iteration < max_iter /*OR other termination criteria*/) {

        _log->info("Iteration %d", _iteration);

        if(rule_weight.empty())
        {
            //! Choose next source.
            _log->info("[ForwardChainer] setting next source");

            _fcmem.set_source(fcb.choose_next_source(_fcmem));

            // Choose matching rules whose input matches with the source.
            vector<Rule*> matched_rules = fcb.choose_rules(_fcmem);

            //! If no rules matches the pattern of the source,
            //! set all rules for candidacy to be selected by the proceeding step.
            //! xxx this decision is made based on recent discussion.
            //! it might still face some changes.
            if (not matched_rules.empty()) {
                _log->info("[ForwardChainer] Found matching rule");

                for (Rule* r : matched_rules) {
                    rule_weight[r] = r->get_weight();
                }

            } else {
                _log->info("[ForwardChainer] No matching rule found. "
                           "Setting all rules as candidates.");
                //TODO subatoms unify here
            }

        }

        UnorderedHandleSet products = do_step(fcb,search_focus_set);

        _fcmem.add_rules_product(_iteration,
                                 HandleSeq(products.begin(), products.end()));
        _fcmem.update_potential_sources(
                HandleSeq(products.begin(), products.end()));

        _iteration++;
    }

    _log->info("[ForwardChainer] finished do_chain.");
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
