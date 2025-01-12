open Graph
open Tools

let rec list_node_dest arcliste acu=
match arcliste with
| [] -> acu
|{src=ids;tgt=idd;_}::rest->list_node_dest rest ((idd,ids)::acu)
;;

let rec is_visited (node,prec:id*id) (visited:(id*id)list)=
match visited with
| [] -> false
| (n,p)::rest -> (n=node) || (is_visited (node,prec) rest);;

(*parcours en largeur, graph source puit*)

let bfs gr s p=
let file =[(s,-1)] in
let visited=[]in
let rec action (liste_n:(id*id)list) (file:(id*id)list)  (visited:(id*id)list) =
  match liste_n with
  |[]->file
  |e::rest-> if (is_visited e visited)=false then (action rest (e::file) visited) else action rest file visited
in
let rec boucle file visited =
  match file with
  |(n,p)::rest ->let nv_file=action (list_node_dest (out_arcs gr n) []) file ((n,p)::visited) in
                boucle (nv_file@rest) ((n,p)::visited)
  |[]->visited
in
boucle file visited
;;

let rec recup_nodes_gr gr acu=match gr with
|(id,_)::rest->recup_nodes_gr	rest (id::acu)
|[]->acu;;

let rec recup_node_visited visited node=
match visited with
|[]->(node,node)
| (n,p)::rest -> if (n=node) then (n,p) else recup_node_visited rest node;;

(*on met pas la source car c'est la meme pour tt l'algo*)
let chemin visited d=
let res=[]in
let rec boucle_while liste node_prec = 
    match node_prec with
    |(x,y)->if x=y then liste else (boucle_while ((x,y)::liste) (recup_node_visited visited y) )
in
let (d2,p2)=recup_node_visited visited d in

boucle_while [(d2,p2)] (recup_node_visited visited p2)
;;

let rec reverseliste liste acu=
match liste with
|[]->acu
|e::rest->reverseliste rest (e::acu);;

(*floyd*)

let rec trouve_arc gr ids idd=
match gr with
|[]->{src=ids;tgt=idd;lbl=0}  
|(_,{src=id;tgt=id2;lbl=a})::rest->if ((id=ids) && (id2=idd)) then {src=id;tgt=id2;lbl=a} else trouve_arc rest ids idd

;;
let recup_label gr s d=(trouve_arc gr s d).lbl;;

let floyd (gr:(id* 'a arc list)list) s p=
  let liste_node=recup_nodes_gr gr [] in
  let gr2=gmap gr (fun x->0) in
  let gr_flot=clone_nodes gr2 in
  let rec boucle_arc liste_arc gr_flot gr=
    match liste_arc with
    |[]->gr_flot
    |{src=ids;tgt=idd;lbl=lbla}::rest->begin
      let valeur=(find_arc gr ids idd).lbl - lbla in 
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
let rec boucle_while parcours gr2 gr_flot s p=
if parcours <>[] then begin  
    let valeur=val_min parcours max_int gr_flot in
    gr2=boucle_parcours parcours valeur gr2;
    gr_flot=boucle_flot (clone_nodes gr2) gr2 liste_node;
    parcours=reverseliste (chemin (bfs gr_flot s p) p) [];
    boucle_while parcours gr2 gr_flot s p;
end
gr2
in
gr_flot=boucle_flot gr_flot gr2 liste_node
parcours= reverseliste (chemin (bfs gr_flot s p) p) []
boucle_while parcours gr2 gr_flot s p
;;



