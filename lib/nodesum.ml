type data = int
type answer = int
type node = { answer : answer; left : int; right : int }

let create : data -> answer = fun data -> data
(* Voir sujet *)

let combine : node -> node -> node =
 (* Combine deux noeuds en faisant la somme des labels, et en récupérant
    le minimum des minorants et le maximum des majorants. *)
 fun left right ->
  {
    answer = left.answer + right.answer;
    left = min left.left right.left;
    right = max left.right right.right;
  }

let to_string : answer -> string =
 (* Suit le format du sujet *)
 fun answer -> string_of_int answer
