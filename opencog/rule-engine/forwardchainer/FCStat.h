/*
 * FCStat.h
 *
 * Copyright (C) 2015 OpenCog Foundation
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

#ifndef _FCSTAT_H_
#define _FCSTAT_H_

#include <opencog/atomspace/AtomSpace.h>

namespace opencog {

class PartiaGroundingRecord {
public:
    const Handle hsource;
    //Partial grounding may not be in any atomspace.
    std::map<Handle, HandleSeq> _rule_pgroundings_map;

    PartiaGroundingRecord(Handle source, Handle hrule, HandleSeq pgroundings) :
                           hsource(source)

    {
        _rule_pgroundings_map[hrule] = pgroundings;
    }

    inline bool operator==(const PartiaGroundingRecord& spg)
    {
        return (spg.hsource == hsource) ? true : false;
    }
};

class InferenceRecord {
public:
    const int step;
    const Handle hsource;
    HandleSeq product;

    InferenceRecord(Handle h, HandleSeq p, int s = 0) :
                  step(s), hsource(h), product(p)
    {
    }

    inline bool operator==(const InferenceRecord& ir)
    {
        return (ir.hsource == hsource) ? true : false;
    }
};

class FCStat {
private:
    std::vector<PartiaGroundingRecord> _spg_stat;
    std::vector<InferenceRecord> _inf_rec;

public:
    bool are_similar(const Handle& h1,const Handle& h2,bool strict_type_match);

    //PartialGroundingRecord queries.
    bool has_partial_grounding(const Handle& hsource);
    void add_partial_grounding(Handle source, Handle hrule, HandleSeq pgroundings);
    std::map<Handle,HandleSeq> get_rule_pg_map(const Handle& hsource);
    HandleSeq get_pg_similar_sources(const Handle& hsource,bool strict);

    //InferenceRecord queries.
    void add_inference_record(Handle source,HandleSeq prodcut);
    HandleSeq get_all_inferences(void);
};

}

#endif /* _FCSTAT_H_ */
