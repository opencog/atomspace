(**
 * query.ml -- A simple query example.
 *
 * Example of performing AtomSpace queries in OCaml.
 *)

(* Populate the AtomSpace with contents *)
evaluation [
	predicate "likely";
	list_link [
		concept "foo" ;
		concept "bar" ]
	] ;;

evaluation [
	predicate "perhaps";
	list_link [
		concept "sunny" ;
		concept "outdoors" ]
	] ;;

(* Take a look at the atomspace *)
cog_print_atomspace ()  ;;

(* Build a query *)
let qry =
	query [
		variable_list [
			typed_variable [
				variable "?x" ;
				type_node "Concept" ]] ;
		present [
			evaluation [
				predicate "perhaps";
				list_link [
					variable "?x" ;
					concept "outdoors" ]]]] ;;


(* That's all folks! *)
