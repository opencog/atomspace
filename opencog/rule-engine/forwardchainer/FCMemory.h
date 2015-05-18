/*
 * FCMemory.h
 *
 * Copyright (C) 2014 Misgana Bayetta
 *
 * Author: Misgana Bayetta <misgana.bayetta@gmail.com>   2015
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
#ifndef FCMEMORY_H_
#define FCMEMORY_H_

#include <opencog/rule-engine/Rule.h>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog {

struct Inference {
	int iter_step;
	Rule* applied_rule;
	HandleSeq inf_product;
	HandleSeq matched_nodes; /**<matched nodes with the variables in the rule,useful during mutual exclusion checking*/
};

class FCMemory {
private:
	bool _search_in_af;
	vector<Rule*> _rules; /*<loaded rules*/
	Rule* _cur_rule;
	Handle _cur_source;

	HandleSeq _selected_sources; /*<selected sources on each forward chaining steps*/
	HandleSeq _potential_sources; /*<list of inference products and premises to select source from*/
	vector<Inference> _inf_history; /*<inference history*/

	AtomSpace* _as;
public:
	FCMemory(AtomSpace* as);
	~FCMemory();

	//Rules
	vector<Rule*>& get_rules();
	void set_rules(vector<Rule*> rules);
	Rule* get_cur_rule();
	void set_cur_rule(Rule* r);

	//Source
	void set_source(Handle source);
	HandleSeq get_selected_sources(void);
	bool isin_selected_sources(Handle h);

	//Potential sources
	HandleSeq get_potential_sources(void);
	void update_potential_sources(HandleSeq input);
	bool isin_potential_sources(Handle h);
	Handle get_cur_source(void);

    //Attentional focus flag
	void set_search_in_af(bool val);
	bool is_search_in_af();

	//History
	void add_rules_product(int iteration, HandleSeq product);
	void add_inference(int iteration, HandleSeq product,
			HandleSeq matched_nodes);
	vector<Inference>& get_inf_history();
	HandleSeq get_result();
	vector<Rule*> get_applied_rules(void);

};

} // ~namespace opencog

#endif /* FCMEMORY_H_ */
