
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>

using namespace opencog;

class AtomComparator {
public:
    inline bool operator()(const AtomPtr& a, const AtomPtr& b) const
        { return this->test(a,b); }
    virtual bool test(const AtomPtr&, const AtomPtr&) const = 0;
};

    //! functor for comparing atom's attention value
    struct STISort : public AtomComparator  {
        STISort() {};
        virtual bool test(const AtomPtr&, const AtomPtr&) const;
    };

    //! functor for comparing atom's attention value
    struct LTIAndTVAscendingSort : public AtomComparator  {
        LTIAndTVAscendingSort() {};
        virtual bool test(const AtomPtr&, const AtomPtr&) const;
    };

    //! functor for comparing atom's attention value
    struct LTIThenTVAscendingSort : public AtomComparator {
        LTIThenTVAscendingSort() {};
        virtual bool test(const AtomPtr&, const AtomPtr&) const;
    };

bool STISort::test(const AtomPtr& h1, const AtomPtr& h2) const
{
    return h1->getAttentionValue()->getSTI() >
           h2->getAttentionValue()->getSTI();
}

bool LTIAndTVAscendingSort::test(const AtomPtr& h1, const AtomPtr& h2) const
{
    AttentionValue::lti_t lti1, lti2;
    float tv1, tv2;

    tv1 = fabs(h1->getTruthValue()->getMean());
    tv2 = fabs(h2->getTruthValue()->getMean());

    lti1 = h1->getAttentionValue()->getLTI();
    lti2 = h2->getAttentionValue()->getLTI();

    if (lti1 < 0)
        tv1 = lti1 * (1.0f - tv1);
    else
        tv1 = lti1 * tv1;

    if (lti2 < 0)
        tv2 = lti2 * (1.0f - tv2);
    else
        tv2 = lti2 * tv2;

    return tv1 < tv2;
}

bool LTIThenTVAscendingSort::test(const AtomPtr& h1, const AtomPtr& h2) const
{
    AttentionValue::lti_t lti1, lti2;
    lti1 = h1->getAttentionValue()->getLTI();
    lti2 = h2->getAttentionValue()->getLTI();

    if (lti1 != lti2) return lti1 < lti2;

    float tv1, tv2;
    tv1 = h1->getTruthValue()->getMean();
    tv2 = h2->getTruthValue()->getMean();
    return tv1 < tv2;
}
