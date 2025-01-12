open Graph

let rec list_node_dest arcliste acu=
match arcliste with
| [] -> acu
|{ids,idd,_}::rest->list_node_dest rest ((idd,ids)::acu)
;;

let rec is_visited node visited=
match visited with
| [] -> false
| (n,p)::rest -> (n=node) || (is_visited node rest)

(*parcours en largeur, graph source puit*)

let bfs gr s p=
let file =[(s,-1)] in
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
| (n,p)::rest -> if (n=node) then (n,p) else recup_node_visited rest node;;

(*on met pas la source car c'est la meme pour tt l'algo*)
let rec chemin visited d=
let (d2,p2)=recup_node_visited visited d in
(d2,p2)::chemin visited d2

(*floyd*)

let recup label gr s d=(find_arc gr s d).lbl

let floyd gr s p=
  let liste_node=recup_nodes_gr gr [] in
  let gr2=gr in
  let gr_flot=clone_nodes gr2 in
  let rec boucle_arc liste_arc gr_flot gr=
    match liste_arc with
    |[]->gr_flot
    |{ids,idd,lbla}::rest->begin
      let valeur=(find_arc gr ids idd).lbl - lbla in ;
      if valeur <> 0 then gr_flot=add_arc gr_flot ids idd valeur;
      if lbla <> 0 then gr_flot=add_arc gr_flot idd ids (-lbla);
      boucle_arc rest gr_flot gr;
    end
in
  let rec boucle_flot gr_flot gr2 liste_node =
    match liste_node with
    |[]->gr_flot
    |id::rest->boucle_flot (boucle_arc (out_arcs gr2 id) gr_flot gr) gr2 rest
in
  let rec boucle_parcours parcours valeur gr2=
    match parcours with
    |(n,-1)::[]->gr2
    |(n,p)::rest->boucle_parcours rest valeur (add_arc gr2 p n ( (find_arc gr_flot p n).lbl/(abs (find_arc gr_flot p n).lbl ) ) *valeur )
in
  let rec val_min parcours v_min gr_flot=
    match parcours with
    |[]->v_min
    |(n,p)::rest->val_min rest (min v_min (abs (find_arc gr_flot p n).lbl) )
in
gr_flot=boucle_flot gr_flot gr2 liste_node
parcours=chemin (bfs gr_flot s p) p
while parcours <> [] do
  begin
    valeur=val_min parcours max_int gr_flot;
    gr2=boucle_parcours parcours valeur gr2;
    gr_flot=boucle_flot (clone_nodes gr2) gr2 liste_node;
    parcours=chemin (bfs gr_flot s p) p;
  end
done
gr2



