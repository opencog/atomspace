/*
 * opencog/atoms/truthvalue/TTruthValue.h
 *
 * Written by Anatoly Belikov <abelikov@singularitynet.io>
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

#ifndef _OPENCOG_TORCH_TRUTH_VALUE_H_
#define _OPENCOG_TORCH_TRUTH_VALUE_H_

#include <opencog/atoms/truthvalue/TruthValue.h>
#include <opencog/atoms/value/PtrValue.h>

#ifndef PyObject_HEAD
struct _object;
typedef _object PyObject;
#endif


namespace opencog
{

/*
 * class TTruthValue holds pointer to torch.Tensor wrapper.
 * methods such as get_mean, get_confidence call this python wrapper.
 * Otherwise it is similiar to SimpleTruthValue.
 */

class TTruthValue: public TruthValue
{
private:
    PyObject * ptr;

    float getAttr(std::string attrname) const;
public:
    TTruthValue(const TTruthValue &) = delete;
    TTruthValue(PyObject * p);
    virtual strength_t get_mean()  const;
    virtual confidence_t get_confidence()  const;
    virtual count_t get_count()  const;

    virtual TruthValuePtr clone() const;
    virtual bool operator==(const Value&) const;
    virtual TruthValuePtr merge(const TruthValuePtr& other,
                                const MergeCtrl& mc) const;
    virtual ~TTruthValue();
    virtual void * getPtr();

};

typedef std::shared_ptr<TTruthValue> TTruthValuePtr;
template<typename ... Type>
static inline TTruthValuePtr createTTruthValue(Type&&...  args) {
   return std::make_shared<TTruthValue>(std::forward<Type>(args)...);
}


}

#endif
