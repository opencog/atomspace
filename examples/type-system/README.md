
Creating Custom Atom Types
==========================
The files here demonstrate how to define new custom Atom Types.

Projects making use of the AtomSpace often require that some new kind
of data to be represented that does not fit into the basic set of
predefined types. The example used here is chemistry, with the new types
consisting of chemical element names, molecular bonds types and
molecules.

The example is split into two parts: defining the new Atom Types, in
the `demo-types` directory, and examples of using them, in the `apps`
directory.

To create new custom Atom Types, just copy these files to your project,
adjust the various file paths, define the desired types, and build.
That's all there's to it!

Boilerplate
-----------
Almost everything in this demo consists of "boilerplate": a conventional,
fixed pattern containing various configurable file names, file paths and
build paramters. Each of these has to be modified to suit your project,
while maintaining the overall structure.  (Boilerplate is the metal plate
of specifications that is riveted to a steam boiler or other appliance.
You can also think of this code as a "magic incantation": as long as you
stick to the general formula, magic will happen, and everything will be
built correctly.)

The most infuriating part of modifying this to suit your project is that
minor typos in filenames, and forgotten or overlooked stanzas can sink
the entire effort.  All of the names and paths must match precisely,
where-ever they appear.

demo-types Directory
--------------------
The Atom Types are defined in the `chem_types.script` file. This can be
freely edited to specify new types. Everything needed to use them are
autogenerated by the `CMakefile.txt`. This includes the scheme and Python
bindings. For OCaml bindings, see the `ocaml` directory in the atomspace
source code.

The language bindings are exposed in the form of modules: the user only
needs to include the correct module, and the types become usable. The
`demo-types.scm` defines a guile scheme module that exposes the scheme
bindings.  An example of using it is in the `apps` directory.

To build everything here, say `make examples` (in the build dir) and
then `sudo make install-demotypes` to install into the root filesystem.

apps Directory
--------------
Contains short examples of using the demo dypes.

The `hello-chem.scm` demos scheme.