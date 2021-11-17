
(* Every interface has to use this same typedef. Even then,
 * there are weird breakages. I can't figure it out.
 *)
type atom = Atom ;; (* | Node of string | Link of atom list;; *)

(** Signature declarations *)
external cog_print_atomspace : unit -> unit = "print_atomspace" ;;

(**
 * Oh dear. Bug. Each new interface has to duplicate these methods!
 * This is quite ugly & impractical. I do not yet understand how to
 * fix this.
 *)
external cog_execute : atom -> atom = "execute" ;;
external cog_execute : Atoms.atom -> Atoms.atom = "execute" ;;

external atom_sexpr : atom -> string = "atom_to_sexpr" ;;
external atom_sexpr : Atoms.atom -> string = "atom_to_sexpr" ;;

external atom_printer : atom -> string = "atom_string_printer" ;;
external atom_printer : Atoms.atom -> string = "atom_string_printer" ;;
