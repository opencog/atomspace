# Cythonizes one .pyx file into a .cpp file
# Additional arguments are dependencies

MACRO(CYTHON_ADD_MODULE_PYX name)
    SET(DEPENDS ${name}.pyx)

    IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${name}.pxd)
        SET(DEPENDS ${DEPENDS} ${name}.pxd)
    ENDIF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${name}.pxd)

    # Allow the user to specify dependencies as optional arguments
    SET(DEPENDS ${DEPENDS} ${ARGN})

    ADD_CUSTOM_COMMAND(
        OUTPUT ${name}.cpp
        COMMAND ${CYTHON_EXECUTABLE}
        ARGS ${CYTHON_FLAGS} -I ${PROJECT_BINARY_DIR}
                -I ${CMAKE_CURRENT_SOURCE_DIR} -o ${name}.cpp
                --cplus ${CMAKE_CURRENT_SOURCE_DIR}/${name}.pyx
        DEPENDS ${DEPENDS}
        COMMENT "Cythonizing ${name}.pyx")

    list(APPEND ADDITIONAL_MAKE_CLEAN_FILES "${name}.cpp")
ENDMACRO(CYTHON_ADD_MODULE_PYX)
