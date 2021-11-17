
(* type atom = Atom ;; | Node of string | Link of atom list;; *)

(* Need to open Atoms to get the pretty-printer to use
 * the typedef for Atom
 *)
open Atoms ;;

(** Signature declarations *)
external prtspace : unit -> unit = "print_atomspace" ;;

(** quick hack *)
let prettyp : Format.formatter -> Atoms.atom -> unit =
	let prtr = function
		| atom -> Format.printf "Dude atom\n"
	in
		(fun oport -> prtr)
	;;

(* #install_printer prettyp ;; *)
