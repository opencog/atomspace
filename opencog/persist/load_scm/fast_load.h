/* fast load of atomese in s-expression format
 *
 * Authors: Alexey Potapov
 *          Anatoly Belikov
 */

#ifndef FAST_LOAD_H
#define FAST_LOAD_H

namespace opencog {
    void load_file(std::string fname, AtomSpace& as);
}


#endif // FAST_LOAD_H
