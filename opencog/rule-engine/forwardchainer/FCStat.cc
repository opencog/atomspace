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

#include "FCStat.h"

using namespace opencog;

/**
 * Makes a one to one similarity matching.If the atoms
 * are of type UnorderedLink, does one vs all similarity
 * matching and removes the matched from matching list
 * immediately.
 *
 * @param h1  A handle
 * @param h2  A handle
 * @param strict_type_match A flag telling how type matching should be done.
 *
 * return  A boolean true if similar and false otherwise.
 */
bool FCStat::are_similar(const Handle& h1, const Handle& h2,bool strict_type_match)
{
    if (h1 == h2) return true;

    if (NodeCast(h1) and NodeCast(h2)) {
        if (strict_type_match and (h1->getType() != h2->getType()) )
            return false;
        else
            return true;
    }

    LinkPtr lh1(LinkCast(h1));
    LinkPtr lh2(LinkCast(h2));

    if (lh1 and lh2) {
        if(strict_type_match and (lh1->getType() != lh2->getType()))
        {
         return false;
        }

        HandleSeq hseqh1 = lh1->getOutgoingSet();
        HandleSeq hseqh2 = lh2->getOutgoingSet();

        if (hseqh1.size() != hseqh2.size()) return false;

        //Unordered links should be treated in a special way
        if (classserver().isA(lh1->getType(), UNORDERED_LINK) or classserver().isA(
                lh2->getType(), UNORDERED_LINK)) {

            for (const auto& h1 : hseqh1) {
                for (auto it = hseqh2.begin(); it != hseqh2.end();) {
                    if (are_similar(h1, h2, strict_type_match)) {
                        hseqh2.erase(it);
                        break;
                    }
                }
            }
            //Empty means all has been mapped.Success.
            if (hseqh2.empty()) return true;

            return false;
        }

        for (HandleSeq::size_type i = 0; i < hseqh1.size(); i++) {
            if (not are_similar(hseqh1[i], hseqh2[i], strict_type_match))
                return false;
        }

        return true;
    }

    return false;
}

bool FCStat::has_partial_grounding(const Handle& hsource)
{
    for (const PartiaGroundingRecord& spg : _spg_stat) {
        if (spg.hsource == hsource)
            return true;
    }

    return false;
}

void FCStat::add_partial_grounding(Handle source, Handle hrule, HandleSeq pgroundings)
{
    PartiaGroundingRecord pgr(source,hrule,pgroundings);
    auto it = std::find(_spg_stat.begin(),_spg_stat.end(),pgr);

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
std::map<Handle,HandleSeq> FCStat::get_rule_pg_map(const Handle& hsource)
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
