
type atom = Atom | Node of string | Link of atom list;;

(** Signature declarations *)
external prtspace : unit -> unit = "print_atomspace" ;;
