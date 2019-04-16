/*
 * opencog/atoms/truthvalue/TorchTruthValue.cc
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

#include <opencog/atoms/truthvalue/TorchTruthValue.h>
#include "SimpleTruthValue.h"

#include <Python.h>

using namespace opencog;


TorchTruthValue::TorchTruthValue(PyObject * p):TruthValue(TORCH_TRUTH_VALUE), ptr(p), _count(1){
   PyGILState_STATE state = PyGILState_Ensure();
   Py_INCREF(p);
   PyGILState_Release(state);
};

float TorchTruthValue::getAttr(std::string p_attrname) const{
    PyGILState_STATE state = PyGILState_Ensure();
    PyObject * tensor = (PyObject*)(this->ptr);
    PyObject * const attrname = PyUnicode_FromString(p_attrname.c_str());
    PyObject * res_obj = PyObject_GetAttr(tensor, attrname);
    PyObject * float_obj = nullptr;
    Py_DECREF(attrname);
    bool failed = false;
    double result = -1.0;
    if (res_obj){
        float_obj = PyObject_CallMethod(res_obj, "__float__", NULL);
        if(float_obj) {
            result  = PyFloat_AsDouble(float_obj);
        }
    } else {
        failed = true;
    }
    if(res_obj) Py_DECREF(res_obj);
    if(float_obj) Py_DECREF(float_obj);
    PyGILState_Release(state);
    if(failed)
        throw RuntimeException(TRACE_INFO, "failed to get element of tensor");
    return result;
}


strength_t TorchTruthValue::get_mean()  const {
    return this->getAttr("mean");
}


confidence_t TorchTruthValue::get_confidence()  const {
    return this->getAttr("confidence");
}


count_t TorchTruthValue::get_count()  const {
    return this->_count;
}


TruthValuePtr TorchTruthValue::clone() const {
    PyObject * tensor = (PyObject*)(this->ptr);
    PyGILState_STATE state = PyGILState_Ensure();
    PyObject * res_obj = PyObject_CallMethod(tensor, "clone", NULL);
    PyGILState_Release(state);
    if(res_obj == nullptr){
         throw RuntimeException(TRACE_INFO, "failed to clone object");
    }
    return createTorchTruthValue(res_obj);
}

bool TorchTruthValue::operator==(const Value& other) const {
     return this == &other;
}

TruthValuePtr TorchTruthValue::merge(const TruthValuePtr& other,
                                      const MergeCtrl& mc) const
{
            throw RuntimeException(TRACE_INFO,
                                   "merge is not implemented");
}

TorchTruthValue::~TorchTruthValue(){
    PyGILState_STATE state = PyGILState_Ensure();
    Py_DECREF(this->ptr);
    PyGILState_Release(state);
}

void * TorchTruthValue::getPtr(){
    return this->ptr;
}


