
(** The top-level glue to automate everything for users.
 * Basically, this avoids having the user to type boring
 * stuff at the interpreter prompt.
 *)

(* Make sure we get to the install directory *)
#directory "/usr/local/lib/opencog/ocaml/" ;;

(* Do a hard load. I was unable to get
 * #use "topfind";; #require "atomese" ;;
 * to work correctly *)

#load "atomspace.cma" ;;
open Atomspace ;;
open Atoms ;;
open Storage ;;

(* There is a different printer for each interface!
 * That's because each interface declares a distinct
 * type atom = Atom ;; perhaps this is a bug? Perhaps
 * type atom should appear in it's own mli file?
 *)
#install_printer Atoms.atom_prettyprt ;;
#install_printer Storage.atom_prettyprt ;;
