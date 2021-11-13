
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

ADD_LIBRARY (ocamloc
	CamlWrap.cc
)

TARGET_LINK_LIBRARIES(ocamloc
	atomspace
)

ADD_OCAML_LIBRARY(atomese BYTECODE
	atomese.mli
	atomese.ml
)

#TARGET_LINK_OCAML_LIBRARIES(ocamloc KIND C_OBJECT
#	atomspace
#)

INSTALL_OCAML_INTERFACES(
	atomese
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")
