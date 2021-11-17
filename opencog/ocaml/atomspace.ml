
(* type atom = Atom ;; | Node of string | Link of atom list;; *)

(* Need to open Atoms to get the pretty-printer to use
 * the typedef for Atom
 *)
open Atoms ;;

(** Signature declarations *)
external prtspace : unit -> unit = "print_atomspace" ;;
external prtatom : atom -> unit = "print_atom" ;;

(** quick hack *)
let prettyp : Format.formatter -> Atoms.atom -> unit =
	(fun oport -> prtatom)
	;;

(* #install_printer prettyp ;; *)
