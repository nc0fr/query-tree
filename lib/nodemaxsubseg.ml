type data = int

(* sum    -> somme des éléments de l'intervalle
   prefix -> la valeur maximale du préfixe de l'intervalle
   suffix -> la valeur maximale du suffixe de l'intervalle
   subseg -> la valeur maximale d'un sous-segment de l'intervalle *)
type answer = { sum : int; prefix : int; suffix : int; subseg : int }
type node = { answer : answer; left : int; right : int }

let create : data -> answer =
 fun data -> { sum = data; prefix = data; suffix = data; subseg = data }

let combine : node -> node -> node =
 fun left right ->
  {
    answer =
      {
        sum = left.answer.sum + right.answer.sum;
        (* TODO(nc0): faire .prefix, .suffix et .subseg *)
        prefix = 0;
        suffix = 0;
        subseg = 0;
      };
    left = min left.left right.left;
    right = max left.right right.right;
  }

let to_string : answer -> string =
 (* Suit le format du sujet *)
 fun answer ->
  (* Application max sur N^4 *)
  let max' a1 a2 a3 a4 = max a1 (max a2 (max a3 a4)) in
  string_of_int (max' answer.sum answer.prefix answer.suffix answer.subseg)
