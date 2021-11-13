
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

ADD_LIBRARY (camlatoms
	CamlWrap.cc
)

SET_TARGET_PROPERTIES(camlatoms PROPERTIES
	PREFIX "dll"
	INSTALL_PATH "lib${LIB_DIR_SUFFIX}/opencog/ocaml/"
)

TARGET_LINK_LIBRARIES(camlatoms
	atomspace
)

# This sets "Force link: YES" but this doesn't seem to affect anything.
# SET(CMAKE_OCaml_LINKER_FLAGS "-linkall")

ADD_OCAML_LIBRARY(atomese
	SOURCES
		atomese.mli
		atomese.ml
	C_LIBRARIES
		camlatoms
)

INSTALL_OCAML_TARGETS(
	atomese
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

INSTALL_OCAML_INTERFACES(
	atomese
	atomese
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")

INSTALL (TARGETS camlatoms
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")
