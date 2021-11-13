
# Empty, nothing here yet.

ADD_LIBRARY (ocamloc
	CamlWrap.cc
)

TARGET_LINK_LIBRARIES(ocamloc
	atomspace
)

# INSTALL_OCAML_INTERFACES(<target> interfacename1 ... DESTINATION <dir>)
