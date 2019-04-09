/*
 * opencog/atoms/truthvalue/TTruthValue.cc
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

#include <opencog/atoms/truthvalue/TTruthValue.h>
#include "SimpleTruthValue.h"

#include <Python.h>

using namespace opencog;


TTruthValue::TTruthValue(PyObject * p):TruthValue(TTRUTH_VALUE), ptr(p){
   PyGILState_STATE state = PyGILState_Ensure();
   Py_INCREF(p);
   PyGILState_Release(state);
};

float TTruthValue::getAttr(std::string p_attrname) const{
    PyGILState_STATE state = PyGILState_Ensure();
    PyObject * tensor = (PyObject*)(this->ptr);
    PyObject * const attrname = PyUnicode_FromString(p_attrname.c_str());
    PyObject * res_obj = PyObject_GetAttr(tensor, attrname);
    PyObject * float_obj = nullptr;
    Py_DECREF(attrname);
    bool failed = false;
    double result;
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


strength_t TTruthValue::get_mean()  const {
    return this->getAttr("mean");
}


confidence_t TTruthValue::get_confidence()  const {
    return this->getAttr("confidence");
}


count_t TTruthValue::get_count()  const {
    return 1;
}


TruthValuePtr TTruthValue::clone() const {
    PyObject * tensor = (PyObject*)(this->ptr);
    PyObject * res_obj = PyObject_CallMethod(tensor, "clone", NULL);
    if(res_obj == nullptr){
         throw RuntimeException(TRACE_INFO, "failed to clone object");
    }
    return createTTruthValue(res_obj);
}

bool TTruthValue::operator==(const Value& other) const {
     return this == &other;
}

TruthValuePtr TTruthValue::merge(const TruthValuePtr& other,
                                      const MergeCtrl& mc) const
{
            throw RuntimeException(TRACE_INFO,
                                   "merge is not implemented");
}

TTruthValue::~TTruthValue(){
    PyGILState_STATE state = PyGILState_Ensure();
    Py_DECREF(this->ptr);
    PyGILState_Release(state);
}

void * TTruthValue::getPtr(){
    return this->ptr;
}


