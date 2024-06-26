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
  let max' a1 a2 a3 = max a1 (max a2 a3) in

  {
    answer =
      (* Pour chaque champ, on prend le maximum entre la valeur du noeud gauche
         et celle du noeud droit lorsqu'il est sommé avec le noeud gauche. *)
      {
        sum = left.answer.sum + right.answer.sum;
        prefix = max left.answer.prefix (left.answer.sum + right.answer.prefix);
        suffix = max right.answer.suffix (right.answer.sum + left.answer.suffix);
        subseg =
          max' left.answer.subseg right.answer.subseg
            (left.answer.suffix + right.answer.prefix);
      };
    left = min left.left right.left;
    right = max left.right right.right;
  }

let to_string : answer -> string =
 (* Suit le format du sujet, le subseg est forcément le max. *)
 fun answer -> string_of_int answer.subseg
