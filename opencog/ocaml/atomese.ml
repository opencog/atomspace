
type atom = Node | Link ;;

(** Signature declarations *)
external newnode : string -> atom = "NewNode" ;;
