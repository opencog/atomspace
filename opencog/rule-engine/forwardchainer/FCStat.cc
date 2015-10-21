/*
 * FCStat.cc
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
#include <opencog/atomutils/AtomUtils.h>

#include "FCStat.h"

using namespace opencog;

bool FCStat::has_partial_grounding(const Handle& hsource)
{
    for (const PartiaGroundingRecord& spg : _spg_stat) {
        if (spg.hsource == hsource)
            return true;
    }

    return false;
}

void FCStat::add_partial_grounding(Handle source, Handle hrule, HandleWeightMap pgroundings)
{
    if (hrule == Handle::UNDEFINED) {
        PartiaGroundingRecord pgr(source);
        _spg_stat.push_back(pgr);
        return;
    }

    PartiaGroundingRecord pgr(source, hrule, pgroundings);
    auto it = std::find(_spg_stat.begin(), _spg_stat.end(), pgr);

    if (it != _spg_stat.end()) {
        (*it)._rule_pgroundings_map.insert(pgr._rule_pgroundings_map.begin(),
                                           pgr._rule_pgroundings_map.end());
    } else {
        _spg_stat.push_back(pgr);
    }
}

/**
 * Given a source handle returns a map of rules which matched source to
 * their partial groundings list.
 *
 * @param hsource  The source handle
 *
 * @return  A map or rules to their partial groundings.
 */
std::map<Handle,HandleWeightMap> FCStat::get_rule_pg_map(const Handle& hsource)
{
    for (PartiaGroundingRecord& pgr : _spg_stat) {
        if (pgr.hsource == hsource)
            return pgr._rule_pgroundings_map;
    }

    return {};
}

/**
 * Given a source handle, tries to find if there were source
 * partial groundings done for structurally similar sources
 * in the past and returns a HandleSeq of those similar sources.
 *
 * @param hsource  A handle to a source
 *
 * @return  A handleSeq of similar sources that have partialgrouding
 *          record.
 */
HandleSeq FCStat::get_pg_similar_sources(const Handle& hsource,bool strict)
{
    HandleSeq simsources = { };

    for (const auto& spg : _spg_stat) {
        if (are_similar(spg.hsource, hsource,strict))
            simsources.push_back(spg.hsource);
    }

    return simsources;
}

void FCStat::add_inference_record(Handle source,HandleSeq product)
{
    InferenceRecord ir(source, product);
    auto it = std::find(_inf_rec.begin(), _inf_rec.end(), ir);

    if (it != _inf_rec.end()) {
        for(Handle h:product)
        {
            HandleSeq& pd = (*it).product;
            if(std::find(pd.begin(),pd.end(),h) == pd.end())
                pd.push_back(h);
        }
    } else {
        _inf_rec.push_back(ir);
    }
}

HandleSeq FCStat::get_all_inferences(void){
  HandleSeq all={};
  for(const auto& ir : _inf_rec)
  {
      all.insert(all.end(),ir.product.begin(),ir.product.end());
  }

  return all;
}
