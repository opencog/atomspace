
type atom = Node of string | Link  of atom list;;

(** Signature declarations *)
external newfoo : string -> atom = "NewNode" ;;
