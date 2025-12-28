
/* Grab the python GIL using RAII style */

class GILGuard {
    PyGILState_STATE gstate;
public:
    GILGuard() : gstate(PyGILState_Ensure()) {}
    ~GILGuard() { PyGILState_Release(gstate); }
    GILGuard(const GILGuard&) = delete;
    GILGuard& operator=(const GILGuard&) = delete;
};
