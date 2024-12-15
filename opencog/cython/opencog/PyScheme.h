//
#ifndef _OPENCOG_PYTHON_SCHEME_H
#define _OPENCOG_PYTHON_SCHEME_H

#include <string>
#include <opencog/atomspace/AtomSpace.h>

namespace opencog
{

/** For easier wrapping by Cython */
std::string eval_scheme(AtomSpace*, const std::string &);
ValuePtr eval_scheme_v(AtomSpace*, const std::string &);
Handle eval_scheme_h(AtomSpace*, const std::string &);
ValuePtr eval_scheme_as(const std::string &);

} // namespace opencog

#endif // _OPENCOG_PYTHON_SCHEME_H

