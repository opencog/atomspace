
(* type atom = Atom ;; | Node of string | Link of atom list;; *)

(* Need to open Atoms to get the pretty-printer to use
 * the typedef for Atom
 *)
open Atoms ;;

(** Signature declarations *)
external prtspace : unit -> unit = "print_atomspace" ;;
external prtatom : atom -> unit = "print_atom" ;;

external atom_printer : Format.formatter -> atom -> unit = "atom_pretty_printer" ;;

(* For some reason, installing atom_printer directly core dumps,
 * so the below is a wrapper around that. Do this:
 * #install_printer atom_prettyprt ;;
 *)
let atom_prettyprt : Format.formatter -> Atoms.atom -> unit =
	(fun oport -> atom_printer oport) ;;
