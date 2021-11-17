
(* type atom = Atom ;; | Node of string | Link of atom list;; *)

(* Need to open Atoms to get the pretty-printer to use
 * the typedef for Atom
 *)
open Atoms ;;

(** Signature declarations *)
external prtspace : unit -> unit = "print_atomspace" ;;
external prtatom : atom -> unit = "print_atom" ;;
external atom_sexpr : atom -> string = "atom_to_sexpr" ;;
external atom_printer : atom -> string = "atom_string_printer" ;;

(* #install_printer atom_prettyprt ;; *)
(*
let atom_prettyprt : Format.formatter -> atom -> unit =
	function oport ->
		fun atm -> Format.fprintf oport "%s" (atom_printer atm) ;;
*)
