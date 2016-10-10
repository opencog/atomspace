#include <opencog/util/Config.h>
#include <opencog/util/exceptions.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cython/PythonEval.h>

#include <iostream>

#include "Utilities.h"

using namespace opencog;


void opencog::initialize_opencog(AtomSpace* atomSpace, const char* configFile)
{
    // Load the config file if one was passed in.
    if (configFile)
    {
        // Load the config file.
        config().load(configFile);
        logger().debug("initialize_opencog - config file loaded");
    }

    // Initialize Python.
    logger().debug("initialize_opencog - initializing Python");
    global_python_initialize();

    // Tell the python evaluator to create its singleton instance
    // with our atomspace.
    logger().debug("initialize_opencog - creating PythonEval singleton instance");
    PythonEval::create_singleton_instance(atomSpace);
}

void opencog::finalize_opencog()
{
    // Delete the singleton instance of the PythonEval.
    PythonEval::delete_singleton_instance();

    // Cleanup Python.
    global_python_finalize();
}

void opencog::configuration_load(const char* configFile)
{
    if (configFile)
        Config().load(configFile);
}
