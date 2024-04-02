open Sig

module Make (N : NODE) : QUERY_STRUCTURE = struct
  type tree = N.node Sig.binary_tree
  type data = int
  type answer = N.answer

  (* Nous déléguons cette résponsabilité aux noeuds, qui eux vont suivre les
       demandes de l'énoncé. *)
  let to_string = N.to_string

  (* Création de l’arbre *)
  let create : data list -> tree =
   fun list ->
    let make_inner_node : int -> N.node =
     (* Créer un noeud interne à partir de la position s d'un élément
        de la liste de donnée *)
     fun s -> { answer = N.create (List.nth list s); left = s; right = s }
    in
    let extract_inner_node : tree -> N.node =
     (* Retourne le noeud interne N.node de notre arbre *)
     fun tree -> match tree with Leaf { node } | Node { node; _ } -> node
    in
    let rec helper : int -> int -> tree =
     (* Fonction récursive qui construit un arbre à partir d'une liste en
        utilisant le principe des doubles pointeurs et de diviser pour régner :
        une liste est passé en paramètre ainsi que les bornes des indices sur
        lesquels nous devons itérer. *)
     fun start_i end_i ->
      if start_i = end_i then
        (* Si la liste ne contient qu'un seul élement (nos deux pointeurs sont
           les mêmes) alors nous pouvons simplement construire une feuille de
           l'arbre.

                         Milieu
                           │
                           │
                           ┌─────────────────────────────────┐
                           │                                 ▼
                          ▼▼▼           i=6       +----------------+
             +---+---+---+---+---+---+---+        |    Node : D    |
             | A | B | C | D | E | F | G |        |    Left : 3    |
             +---+---+---+---+---+---+---+        |    Right : 3   |
            i=0           ▲▲▲                     +----------------+
                           │                                 ▲
                           └─────────────────────────────────┘
                           │
                           │
        *)
        Leaf { node = make_inner_node start_i }
      else
        (* Sinon, nous devons diviser la liste en deux par son milieu, pour
           obtenir un sous-arbre de gauche et un de droite, puis les
           combiner.

                         Milieu
                           │
                   ┌───────────────────────────────────────┐
                   │       │                               ▼
                   │       │
                   │       │                                  i=3
                   │       │            i=6        +---+---+---+
             +---+---+---+---+---+---+---+         | A | B | C |        ...
             | A | B | C | D | E | F | G |         +---+---+---+
             +---+---+---+---+---+---+---+        i=0
            i=0            │       │
                           │       └────────────────────────┐
                           │                                ▼
                           │                                      i=6
                           │                       +---+---+---+---+
                           │                       | D | E | F | G |    ...
                           │                       +---+---+---+---+
                           │                      i=3
                           │
        *)
        let middle_i = (start_i + end_i) / 2 in
        let left_child = helper start_i middle_i in
        let right_child = helper (middle_i + 1) end_i in
        let node =
          N.combine
            (extract_inner_node left_child)
            (extract_inner_node right_child)
        in
        Node { node; left_child; right_child }
    in
    helper 0 (List.length list - 1)

  (* Mise à jour d’un élément de la liste *)
  let update : tree -> data -> int -> tree =
   fun tree data i ->
    let extract_inner_node : tree -> N.node =
     (* Retourne le noeud interne N.node de notre arbre *)
     fun tree ->
      match tree with Leaf { node } -> node | Node { node; _ } -> node
    in
    let rec helper : tree -> tree =
     (* Corps récursif *)
     fun tree ->
      match tree with
      | Leaf { node = _ } ->
          (* Update sur une feuille oblige de changer la donnée de cette
             feuille.  Et car notre récurssion est vérifiée, nous savons que
             l'intervalle [i; i] reste correct *)
          Leaf { node = { answer = N.create data; left = i; right = i } }
      | Node { node; left_child; right_child } ->
          (* Update sur un noeud demande de faire un nouveau noeud dont l'un
             des enfants sera modifié (selon la position à modifier).
             Nous adoptons encore une fois une stratégie de diviser pour mieux
             régner. *)
          let middle_i = (node.left + node.right) / 2 in
          let combine_trees : tree -> tree -> tree =
           (* Combine deux sous-abres ensembles.

                                                       +-----+
                                                       | A+B |
                                                       |[0;1]|
                                                       +--+--+
                                                          │
                                                     ┌────┴────┐
                                                     ▼         ▼
                             +-----+  +-----+     +-----+   +-----+
                             |  A  |  |  B  |     |  A  |   |  B  |
              combine_trees  |[0;0]|  |[1;1]|  =  |[0;0]|   |[1;1]|
                             +-----+  +-----+     +-----+   +-----+
           *)
           fun left_child right_child ->
            let left_inner_node = extract_inner_node left_child in
            let right_inner_node = extract_inner_node right_child in
            let node = N.combine left_inner_node right_inner_node in
            Node { node; left_child; right_child }
          in
          let left, right =
            if i <= middle_i then
              (* Si le la position à modifier est dans la première moitié de
                 notre intervalle (et donc liste), alors on sait que la
                 modification sera fera dans le sous-arbre de gauche. *)
              (helper left_child, right_child)
            else
              (* Sinon, c'est forcément à droite :) *)
              (left_child, helper right_child)
          in
          combine_trees left right
    in
    helper tree

  let rec query : tree -> int -> int -> answer =
   fun tree left right ->
    (* On commence par décomposer le noeud courant, cela nous permet de
       déterminer les choix à notre disposition. *)
    match tree with
    | Leaf { node } ->
        (* Soit nous sommes sur une feuille, auquel cas notre intervalle doit
           correspondre à celui de la feuille et la feuille est notre résultat.
           Si cela n'est pas le cas, alors l'intervalle demandé est incorrect
           de base. *)
        if node.left = left && node.right = right then node.answer
        else failwith "Invalid interval"
    | Node { node; left_child; right_child } ->
        (* Soit nous sommes sur un noeud parent, auquel cas nous devons
           déterminer la suite de l'algorithme en comparant l'intervalle
           du noeud courant à l'intervalle donné. *)
        if node.left = left && node.right = right then
          (* Premier cas possible, l'intervalle du noeud courant est
             exactement le même que celui demandé. *)
          node.answer
        else
          (* Deuxième cas possible, l'intervalle du noeud courant est
             plus à droite (resp. à gauche) que celui demandé.
             Nous pouvons déterminer la notion de "droite" et "gauche" en
             comparant les bornes de l'intervalle donnée avec le milieu de
             l'intervalle du noeud courant. *)
          let middle = (node.left + node.right) / 2 in
          if right <= middle then
            (*
                                           Milieu
                                             ▲
                                             │
                                             │
                               +---+---+---+---+---+---+---+
              Noeud courant    | A | B | C | D | E | F | G |
                               +---+---+---+---+---+---+---+
                                             │
                               +---+---+---+---+
              Intervalle donné | A | B | C | D |
                               +---+---+---+---+
                                             │
                                             │
                                             ▼

              Nous sommes à gauche du noeud courant, nous devons donc chercher
              dans le sous-arbre de gauche. *)
            query left_child left right
          else if left > middle then
            (*
                                           Milieu
                                             ▲
                                             │
                                             │
                               +---+---+---+---+---+---+---+
              Noeud courant    | A | B | C | D | E | F | G |
                               +---+---+---+---+---+---+---+
                                             │
                                           +---+---+---+---+
              Intervalle donné             | D | E | F | G |
                                           +---+---+---+---+
                                             │
                                             │
                                             ▼

              Nous sommes à droite du noeud courant, nous devons donc chercher
              dans le sous-arbre de droite. *)
            query right_child left right
          else
            (*
                                           Milieu
                                             ▲
                                             │
                                             │
                               +---+---+---+---+---+---+---+
              Noeud courant    | A | B | C | D | E | F | G |
                               +---+---+---+---+---+---+---+
                                             │
                                       +---+---+---+---+
              Intervalle donné         | C | D | E | F |
                                       +---+---+---+---+
                                             │
                                             │
                                             ▼

              Notre intervalle est sur les deux côtés du noeud courant, nous
              devons donc effectuer la recherche sur les deux côtés et retourner
              leur combinaison. *)
            let query_left = query left_child left middle in
            let query_right = query right_child (middle + 1) right in
            let query_combine =
              N.combine
                { answer = query_left; left; right = middle }
                { answer = query_right; left = middle + 1; right }
            in
            query_combine.answer
end
