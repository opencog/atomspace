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

ForwardChainer::ForwardChainer(AtomSpace * as, const string& conf_path) :
		_as(as), _rec(_as), _conf_path(conf_path), _fcmem(_as) {
	init();
}

ForwardChainer::~ForwardChainer() {
	delete _cpolicy_loader;
}

void ForwardChainer::init() {
	_cpolicy_loader = new JsonicControlPolicyParamLoader(_as, _conf_path);
	_cpolicy_loader->load_config();
	_fcmem.set_search_in_af(_cpolicy_loader->get_attention_alloc());
	_fcmem.set_rules(_cpolicy_loader->get_rules());
	_fcmem.set_cur_rule(nullptr);

	// Provide a logger
	_log = NULL;
	setLogger(new opencog::Logger("forward_chainer.log", Logger::FINE, true));
}

void ForwardChainer::setLogger(Logger* log) {
	if (_log)
		delete _log;
	_log = log;
}

Logger* ForwardChainer::getLogger() {
	return _log;
}

/**
 * Does one step forward chaining
 *
 * @param fcb a concrete implementation of of ForwardChainerCallBack class 
 */
void ForwardChainer::step(ForwardChainerCallBack& fcb) {

	if (_fcmem.get_cur_source() == Handle::UNDEFINED) {
		_log->info(
				"[ForwardChainer] No current source, step forward chaining aborted.");
		return;
	}

	_log->info("[ForwardChainer] Next source %s",
			_fcmem.get_cur_source()->toString().c_str());

	// Choose matching rules whose input matches with the source.
	vector<Rule*> matched_rules = fcb.choose_rules(_fcmem);

	//! If no rules matches the pattern of the source,
	//! set all rules for candidacy to be selected by the proceeding step.
	//! xxx this decision is maded based on recent discussion.I
	//! it might still face some changes.
	if (matched_rules.empty()) {
		_log->info(
				"[ForwardChainer] No matching rule found. Setting all rules as candidates.");
		matched_rules = _fcmem.get_rules();
	}

	// Select a rule amongst the matching rules by tournament selection
	map<Rule*, float> rule_weight;
	for (Rule* r : matched_rules) {
		rule_weight[r] = r->get_cost();
	}

	_log->info(
			"[ForwardChainer] Selecting a rule from the set of candidate rules.");
	auto r = _rec.tournament_select(rule_weight);
	_fcmem.set_cur_rule(r);
	_log->info("[ForwardChainer] Selected rule is %s", r->get_name().c_str());

	//!TODO Find/add premises?

	//! Apply rule.
	_log->info("[ForwardChainer] Applying chosen rule %s",
			r->get_name().c_str());
	HandleSeq product = fcb.apply_rule(_fcmem);

	_log->info(
			"[ForwardChainer] updating premise list with the inference made");
	_fcmem.update_potential_sources(product);

	_log->info("[ForwardChainer] adding inference to history");
	_fcmem.add_rules_product(_iteration, product);
}

void ForwardChainer::do_chain(ForwardChainerCallBack& fcb,
		Handle hsource/*=Handle::UNDEFINED*/) {
	if (hsource == Handle::UNDEFINED) {
		do_pm();
		return;
	}
	// Variable fulfillment query.
	UnorderedHandleSet var_nodes = get_outgoing_nodes(hsource,
			{ VARIABLE_NODE });
	if (not var_nodes.empty())
		return do_pm(hsource, var_nodes, fcb);

	auto max_iter = _cpolicy_loader->get_max_iter();
	_fcmem.set_source(hsource); //set initial source

	while (_iteration < max_iter /*OR other termination criteria*/) {
		_log->info("Iteration %d", _iteration);

		step(fcb);

		//! Choose next source.
		_log->info("[ForwardChainer] setting next source");
		_fcmem.set_source(fcb.choose_next_source(_fcmem));

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
		const UnorderedHandleSet& var_nodes, ForwardChainerCallBack& fcb) {
	DefaultImplicator impl(_as);
	impl.implicand = hsource;
	HandleSeq vars;
	for (auto h : var_nodes)
		vars.push_back(h);
	_fcmem.set_source(hsource);
	Handle hvar_list = _as->addLink(VARIABLE_LIST, vars);
	Handle hclause = _as->addLink(AND_LINK, hsource);

	// Run the pattern matcher, find all patterns that satisfy the
	// the clause, with the given variables in it.
	PatternLinkPtr sl(createPatternLink(hvar_list, hclause));
	sl->satisfy(impl);

	// Update result
	_fcmem.add_rules_product(0, impl.result_list);

	// Delete the AND_LINK and LIST_LINK
	_as->removeAtom(hvar_list);
	_as->removeAtom(hclause);

	//!Additionally, find applicable rules and apply.
	vector<Rule*> rules = fcb.choose_rules(_fcmem);
	for (Rule* rule : rules) {
		BindLinkPtr bl(BindLinkCast(rule->get_handle()));
		DefaultImplicator impl(_as);
		impl.implicand = bl->get_implicand();
		bl->imply(impl);
		_fcmem.set_cur_rule(rule);
		_fcmem.add_rules_product(0, impl.result_list);
	}
}
/**
 * Invokes pattern matcher using each rule declared in the configuration file.
 */
void ForwardChainer::do_pm() {
	//! Do pattern matching using the rules declared in the declaration file
	_log->info(
			"Forward chaining on the entire atomspace with rules declared in %s",
			_conf_path.c_str());
	vector<Rule*> rules = _fcmem.get_rules();
	for (Rule* rule : rules) {
		_log->info("Applying rule %s on ", rule->get_name().c_str());
		BindLinkPtr bl(BindLinkCast(rule->get_handle()));
		DefaultImplicator impl(_as);
		impl.implicand = bl->get_implicand();
		bl->imply(impl);
		_fcmem.set_cur_rule(rule);

		_log->info("OUTPUTS");
		for (auto h : impl.result_list)
			_log->info("%s", h->toString().c_str());

		_fcmem.add_rules_product(0, impl.result_list);
	}

}

HandleSeq ForwardChainer::get_chaining_result() {
	return _fcmem.get_result();
}
