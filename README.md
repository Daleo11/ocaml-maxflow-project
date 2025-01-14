L'algorithme implémenté est l'algorithme de Floyd Fulkerson résolvant le problème de flot maximal pour un graph donné.

L'algorithme fonctionne de la facon suivante :
- initialisation d'un graph g2 avec son flot a 0 (ce sera celui qui sera modifier tout au long de l'algorithme)
 -Faire le graph de flot a partir du graph donné
 -Trouver un chemin de flot augmentant (utilise le parcours bfs)
 -Recuperer la valeur d'augmentation du flot
 -Augmenter/diminuer le flot du graph g2 en suivant le chemin trouvé
- continuer jusqu'a ce qu'il n'y ai plus de chemin
-renvoyer le graph g2 qui est donc le graph avec le flot maximal
-afficher la valeur du flot maximal

le parcours bfs fonctionne de la facon suivante :
- initialisation d'une file avec la node source source
- initialisation d'une liste pour stocker les nodes visitées, avec comme premiere node visitées la source
-on prend le premier element de la file, on regarde ses successeurs si ils sont visité on ne fait rien sinon on les ajoutes dans la file et on les marque comme visités
-on continue jusqu'a ce que la file soit vide
-on renvoie la liste des nodes visités

le format utiliser pour la file et visited est : (id*id)list cad (node,precedente)list
Pour chaque node on garde la node precedente, cela nous permet de retrouver le chemin et les arcs associés
Cas particulier pour la source qui est notée : (source,source)