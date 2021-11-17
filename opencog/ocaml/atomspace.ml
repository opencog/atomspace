
(* Every interface has to use this same typedef. Even then,
 * there are weird breakages. I can't figure it out.
 *)
type atom = Atom ;; (* | Node of string | Link of atom list;; *)

(** Signature declarations *)
external prtspace : unit -> unit = "print_atomspace" ;;
external prtatom : atom -> unit = "print_atom" ;;
external atom_sexpr : atom -> string = "atom_to_sexpr" ;;
external atom_printer : atom -> string = "atom_string_printer" ;;
