type data = int
type answer = int * int (* valeur maximum, occurence de la valeur maximum *)
type node = { answer : answer; left : int; right : int }

let create : data -> answer = fun data -> (data, 1)

let combine : node -> node -> node =
 fun left right ->
  {
    (* On prend la réponse ayant la plus grande valeur maximale.
       Si jamais elles ont les mêmes valeurs, alors il suffit de prendre
       la gauche et de combiner les occurrences. *)
    answer =
      (if fst left.answer > fst right.answer then left.answer
       else if fst left.answer < fst right.answer then right.answer
       else (fst left.answer, snd left.answer + snd right.answer));
    left = min left.left right.left;
    right = max left.right right.right;
  }

let to_string : answer -> string =
 (* Suit le format du sujet *)
 fun answer ->
  "(" ^ string_of_int (fst answer) ^ ", " ^ string_of_int (snd answer) ^ ")"
