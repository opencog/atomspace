(**
 * basics.ml -- The most basic example of using the AtomSpace.
 *
 * Demos some of the basic OCaml API.
 *)

(* Create a foo node *)
concept "foo" ;;

(* Print the AtomSpace contents *)
prtspace ()  ;;

(* Create a few more nodes, and a link *)
let f = concept "foo" ;;
let b = concept "bar" ;;
let l = list_link [ f; b] ;;

(* Take a look, again *)
prtspace ()  ;;

(* Create a rather convetional evaluation link *)
let e = evaluation [ predicate "likely"; l] ;;

(* Get the string s-expression for it *)
atom_sexpr e ;;

(* That's all folks! *)
