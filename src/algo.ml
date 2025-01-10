open Graph

let rec list_node_dest arcliste acu=
match arcliste with
| [] -> acu
|{_,id,_}::rest->list_node_dest rest (id::acu)
;;

let rec is_visited node visited=
match visited with
| [] -> false
| (n,p)::rest -> (n=node) || (is_visited node rest)

(*parcours en largeur, graph source puit*)

let bfs gr s p=
let file =[s] in
let visited=[]in
let action liste_n file visited =
  match liste_n with
  |[]->file
  |e::rest-> if (is_visited e visited)=false then e::file
in
let boucle file visited =
  match file with
  |(n,p)::rest ->boucle (action (list_node_dest (rest.out_arcs))) file ((n,p)::visited) (n,p)::visited
  |[]->visited
in
boucle file visited
;;

let rec recup_nodes_gr gr acu=match gr with
|(id,_)::rest->recup_nodes_gr	rest (id::acu)
|[]->acu;;

let rec recup_node_visited visited node=
match visited with
|[]->[]
| (n,p)::rest -> if (n=node) then (n,p)::rest else recup_node_visited rest node;;

(*on met pas la source car c'est la meme pour tt l'algo*)
let rec chemin visited d=
let (d2,p2)<-recup_node_visited visited d in
(d2,p2)::chemin visited d2

(*floyd*)