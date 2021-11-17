
type atom = Atom ;; (* | Node of string | Link of atom list;; *)

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
