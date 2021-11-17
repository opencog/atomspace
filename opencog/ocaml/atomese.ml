
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

(* The printer is defined in the atomspace.ml file *)
#install_printer prettyp ;;
