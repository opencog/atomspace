from libcpp cimport bool

# basic wrapping for std::string conversion
cdef extern from "<string>" namespace "std":
    cdef cppclass string:
        string()
        string(char *)
        char * c_str()
        int size()


cdef extern from "opencog/util/Logger.h" namespace "opencog":
    # Need to get around cython's lack of support for nested types
    enum loglevel "opencog::Logger::Level":
        NONE "opencog::Logger::NONE"
        ERROR "opencog::Logger::ERROR"
        WARN "opencog::Logger::WARN"
        INFO "opencog::Logger::INFO"
        DEBUG "opencog::Logger::DEBUG"
        FINE "opencog::Logger::FINE"
        BAD_LEVEL "opencog::Logger::BAD_LEVEL"
    cdef cppclass cLogger "opencog::Logger":
        cLogger()
        cLogger(string s)
        void set_level(loglevel lvl)
        void set_component(string c)
        loglevel get_level()
        void set_print_to_stdout_flag(bool flag)

        void log(loglevel lvl, string txt)

        bool is_enabled(loglevel lvl)

    cdef loglevel string_to_log_level "opencog::Logger::get_level_from_string"(string s)
    cdef string log_level_to_string "opencog::Logger::get_level_string"(loglevel lvl)
    cLogger& logger()


cdef class Logger:
    cdef cLogger *clog
    cdef not_singleton_logger
    cdef _set_level(self,int lvl)
    cdef _level_as_string(self)

cdef Logger wrap_clogger(cLogger *clog)
