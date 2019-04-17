/*
 * opencog/atoms/truthvalue/TensorTruthValue.cc
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

#include <opencog/atoms/truthvalue/TensorTruthValue.h>
#include <Python.h>


using namespace opencog;


TensorTruthValue::TensorTruthValue(PyObject * p):TruthValue(TENSOR_TRUTH_VALUE), ptr(p), _count(1){
   PyGILState_STATE state = PyGILState_Ensure();
   Py_INCREF(p);
   PyGILState_Release(state);
};

float TensorTruthValue::getAttr(std::string p_attrname) const{
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


strength_t TensorTruthValue::get_mean()  const {
    return this->getAttr("mean");
}


confidence_t TensorTruthValue::get_confidence()  const {
    return this->getAttr("confidence");
}


count_t TensorTruthValue::get_count()  const {
    return this->_count;
}


TruthValuePtr TensorTruthValue::clone() const {
    PyObject * tensor = (PyObject*)(this->ptr);
    PyGILState_STATE state = PyGILState_Ensure();
    PyObject * res_obj = PyObject_CallMethod(tensor, "clone", NULL);
    PyGILState_Release(state);
    if(res_obj == nullptr){
         throw RuntimeException(TRACE_INFO, "failed to clone object");
    }
    return createTensorTruthValue(res_obj);
}

bool TensorTruthValue::operator==(const Value& other) const {
     return this == &other;
}

TruthValuePtr TensorTruthValue::merge(const TruthValuePtr& other,
                                      const MergeCtrl& mc) const
{
            throw RuntimeException(TRACE_INFO,
                                   "merge is not implemented");
}

TensorTruthValue::~TensorTruthValue(){
    PyGILState_STATE state = PyGILState_Ensure();
    Py_DECREF(this->ptr);
    PyGILState_Release(state);
}

void * TensorTruthValue::getPtr(){
    return this->ptr;
}

std::string TensorTruthValue::to_string(const std::string&) const {
    PyGILState_STATE state = PyGILState_Ensure();
    PyObject * str = PyObject_Str(this->ptr);
    if(str == nullptr){
         PyGILState_Release(state);
         throw RuntimeException(TRACE_INFO, "error calling __str__ on python object");
    }
#if PY_MAJOR_VERSION == 2
    const char * tmp = PyBytes_AsString(str);
#else
    const char * tmp = PyUnicode_AsUTF8(str);
#endif
    std::string result = std::string(tmp);
    Py_DECREF(str);
    PyGILState_Release(state);
    return result;
}
