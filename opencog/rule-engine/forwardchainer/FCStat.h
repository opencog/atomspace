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

#include <opencog/atoms/base/Handle.h>
#include <map>

#include "../Rule.h"

namespace opencog {

struct InferenceRecord
{
    const int step;
    const Handle hsource;
    const Rule* rule;
    HandleSeq product;

    InferenceRecord(Handle h, const Rule* r, const HandleSeq& p, int s = 0) :
        step(s), hsource(h), rule(r), product(p)
    {
    }

    inline bool operator==(const InferenceRecord& ir)
    {
        return (ir.hsource == hsource
                and ir.rule == rule
                and ir.product == product);
    }
};

class FCStat
{
private:
    std::vector<InferenceRecord> _inf_rec;

public:
    // InferenceRecord queries.
    void add_inference_record(Handle source, const Rule* rule,
                              const HandleSeq& product);
    HandleSeq get_all_inferences(void);
};

}

#endif /* _FCSTAT_H_ */
