
# Use UseOCaml.cmake, provided by cogutils
INCLUDE(UseOCaml)

ADD_LIBRARY (ocamloc
	CamlWrap.cc
)

TARGET_LINK_LIBRARIES(ocamloc
	atomspace
)

ADD_OCAML_LIBRARY(atomese
	BYTECODE
		atomese.mli
		atomese.ml
	C_LIBRARIES
		ocamloc
)

INSTALL_OCAML_TARGETS(
	atomese
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog/ocaml")
