/*
 * UREConfigReader.h
 *
 * Copyright (C) 2015 OpenCog Foundation
 *
 * Author: Nil Geisweiller <ngeiswei@gmail.com>
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

#ifndef _URE_CONFIG_READER_H
#define _URE_CONFIG_READER_H

namespace opencog {
	
/**
 * Read the URE configuration in the AtomSpace as described in
 * http://wiki.opencog.org/w/URE_Configuration_Format, and provide
 * parameter accessors for all rule-based systems.
 */
class UREConfigReader
{
public:
	// Ctor
	UREConfigReader(AtomSpace& as);

	// Access methods, return parameters given a rule-based system
	// name.
	const std::vector<Rule*>& get_rules(const std::string& system) const;
	bool get_attention_allocation(const std::string& system) const;
	int get_maximum_iterations(const std::string& system) const;

	// Name of the top rule base from which all rule-based systems
	// inherit. It should corresponds to a ConceptNode in the
	// AtomSpace.
	static const URE_top_name = "URE";
private:

	// Fetch from the AtomSpace all rule based systems (i.e. inheriting
	// ConceptNode URE_top_name).
	HandleSeq fetch_rule_based_systems();

	// Fetch from the AtomSpace all rule names of a given system
	// (i.e. members of that system).
	HandleSeq fetch_rules(Handle rbsys);
	
	AtomSpace& _as;

	// @todo: It doesn't support the hierarchical configuration
	// structure described in
	// http://wiki.opencog.org/w/URE_Configuration_Format#Rule-Based_System_Hierarchical_Structure,
	// instead all parameters must be duplicated for all systems and
	// subsystems, for now.
	class RuleBaseParameters {
	public:
		std::vector<Rule*> rules;
		bool attention_alloc;
		int max_iter;
	};
	map<std::string, RuleBaseParameters> _sys2params;
};
	
} // ~namespace opencog


#endif /* _URE_CONFIG_READER_H_ */
