
#include <opencog/atoms/proto/ProtoAtom.h>
#include <opencog/atoms/proto/FloatValue.h>
#include <opencog/atoms/proto/LinkValue.h>
#include <opencog/atoms/proto/RandomStream.h>
#include <opencog/atoms/proto/StringValue.h>
#include <opencog/atoms/proto/ValueFactory.h>

using namespace opencog;

static __attribute__ ((constructor)) void init(void)
{
    valuefactory().register_factory(LINK_VALUE, (CreateProto) & (createLinkValue<std::vector<ProtoAtomPtr>>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::vector<ProtoAtomPtr>))});

    valuefactory().register_factory(FLOAT_VALUE, (CreateProto) & (createFloatValue<std::vector<double>>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::vector<double>))});

    valuefactory().register_factory(FLOAT_VALUE, (CreateProto) & (createFloatValue<double>),
                                    std::vector<std::type_index> {std::type_index(typeid(double))});

    valuefactory().register_factory(STRING_VALUE, (CreateProto) & (createStringValue<std::vector<std::string>>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::vector<std::string>))});

    valuefactory().register_factory(STRING_VALUE, (CreateProto) & (createStringValue<std::string>),
                                    std::vector<std::type_index> {std::type_index(typeid(std::string))});

    valuefactory().register_factory(RANDOM_STREAM, (CreateProto) & (createRandomStream<int>),
                                    std::vector<std::type_index> {std::type_index(typeid(int))});
}

