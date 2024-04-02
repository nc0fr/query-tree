# Devoir de Programmation Fonctionnelle

> **LE CODE DANS CE DÉPÔT N'EST PAS EXACTEMENT LE MÊME RENDU POUR DES RAISONS
> DE SÉCURITÉ**

Pour installer les paquets dont le projet a besoin :

```shell
opam install .  \
    --deps-only \
    --with-doc  \
    --with-test \
```

Pour compiler le projet :

```shell
dune build
```

Pour lancer les tests :

```shell
dune test
```

Pour exécuter le projet :

```shell
dune exec devoir_de_programmation
```

Pour exécuter le projet avec des arguments de ligne de commande :

```shell
dune exec devoir_de_programmation -- [...args]
```

Exemple :

```shell
dune exec devoir_de_programmation -- \
    -data "1 2 3 2"                  \
    -problem maxocc                  \
    -print                           \
    -query 0 1                       \
    -query 0 3                       \
    -update 3 0                      \
    -print                           \
    -query 0 3
```

devrait donner l'affichage suivant :

```shell
(1, 1) (2, 1) (3, 1) (2, 1)
(2, 1)
(3, 1)
input[0]=3
(3, 1) (2, 1) (3, 1) (2, 1)
(3, 2)
```
