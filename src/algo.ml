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

let recup_nodes_gr gr =
n_fold gr (fun acu e ->e::acu) [] 
;;

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
match (find_arc gr ids idd) with
  |None->{src=idd ;tgt=idd ;lbl=0}
  |Some arci-> {src=arci.src ;tgt=arci.tgt ;lbl=arci.lbl};;

;;
let recup_label gr s d=(trouve_arc gr s d).lbl;;

(*let gmap2 gr f=
let transfo g arc=new_arc g {src = arc.src ;tgt=arc.tgt; lbl=(f arc.lbl) }in
let rec boucle_arc g2 liste_arc=
  match liste_arc with
  |[]->g2
  |e::rest->boucle_arc (transfo g2 e) rest 
in
let rec boucle_node (gr:'a graph) g2=
  match gr with
  |[]->g2
  |(_,listeoutarc)::rest->boucle_node rest (boucle_arc g2 listeoutarc)
in
let g2=clone_nodes gr in
boucle_node gr g2
;;*)

let floyd gr s p=
  let liste_node=recup_nodes_gr gr in
  let gr2=gmap gr (fun x->0) in
  let gr_flot=clone_nodes gr2 in
  
  let rec boucle_arc1 liste_arc gr_flot gr=
    match liste_arc with
    |[]->gr_flot
    |{src=ids;tgt=idd;lbl=lbla}::rest->begin
      let valeur=(trouve_arc gr ids idd).lbl - lbla in 
      if valeur <> 0 then (boucle_arc1 rest (add_arc gr_flot ids idd valeur) gr) else (boucle_arc1 rest gr_flot gr)
    end
in
let rec boucle_arc2 liste_arc gr_flot gr=
    match liste_arc with
    |[]->gr_flot
    |{src=ids;tgt=idd;lbl=lbla}::rest->
      if lbla <> 0 then (boucle_arc2 rest (add_arc gr_flot idd ids (-lbla)) gr) else (boucle_arc2 rest gr_flot gr)

in
let boucle_arc liste_arc gr_flot gr=
  boucle_arc2 liste_arc (boucle_arc1 liste_arc gr_flot gr) gr
in
  let rec boucle_flot gr_flot gr2 liste_node =
    match liste_node with
    |[]->gr_flot
    |id::rest->boucle_flot (boucle_arc (out_arcs gr2 id) gr_flot gr) gr2 rest
in
  let rec boucle_parcours parcours valeur gr2=
    match parcours with
    |[]->gr2
    |(n,p)::rest->boucle_parcours rest valeur (add_arc gr2 p n (( (trouve_arc gr_flot p n).lbl/(abs (trouve_arc gr_flot p n).lbl ) ) *valeur ))

  in
  let rec val_min parcours v_min gr_flot=
    match parcours with
    |[]->v_min
    |(n,p)::rest->val_min rest (min v_min (abs (trouve_arc gr_flot p n).lbl) ) gr_flot
in
let rec boucle_while parcours gr2 gr_flot s p=
if parcours <>[] then begin  
    let valeur=val_min parcours max_int gr_flot in
    let gr2=boucle_parcours parcours valeur gr2 in
    let gr_flot=boucle_flot (clone_nodes gr2) gr2 liste_node in
    let parcours=reverseliste (chemin (bfs gr_flot s p) p) [] in
    boucle_while parcours gr2 gr_flot s p;
  end
else gr2
in
let gr_flot=boucle_flot gr_flot gr2 liste_node in
let parcours= reverseliste (chemin (bfs gr_flot s p) p) [] in
boucle_while parcours gr2 gr_flot s p
;;



