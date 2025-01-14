open Graph
open Tools
(*open Gfile*)

(*renvois une liste (node,precedente)list des nodes issue de la liste d'arc*)
let rec list_node_dest arcliste acu=
match arcliste with
| [] -> acu
|{src=ids;tgt=idd;_}::rest->list_node_dest rest ((idd,ids)::acu)
;;

(*renvoi true si la node a ete visite false sinon*)
let rec is_visited (node,prec:id*id) (visited:(id*id)list)=
match visited with
| [] -> false
| (n,_)::rest ->(n=node) || (is_visited (node,prec) rest);;

(*parcours en largeur, graph source*)
(*renvois une liste (node,precedentes)list de toute les nodes atteignable en partant de la source*)
let bfs gr s=
let file =[(s,s)] in
let visited=[(s,s)]in
let rec action (liste_n:(id*id)list) (file:(id*id)list)  (visited:(id*id)list) = (*pour chaque node de liste_n si elle n'est pas visite on la met dans la file sinon rien*)
match liste_n with
  |[]->(file,visited)
  |(n,p)::rest->if (is_visited (n,p) visited)=false then (action rest ((n,p)::file) ((n,p)::visited)) else action rest file visited
in
let rec boucle file visited =(*prend la premiere node de la file, regarde ses destination et les ajoute dans la file si elles ne sont pas visited (utilisation de action pour la decision)*)
  match file with
  |(n,_)::rest ->let (nv_file,visited)=action (list_node_dest (out_arcs gr n) []) [] visited in
                boucle (nv_file@rest) (visited)
  |[]->visited
in
boucle file visited
;;

(*renvois une liste de toute les nodes du graph*)
let recup_nodes_gr gr =
n_fold gr (fun acu e ->e::acu) [] 
;;

(*renvois le duo (n,prec) avec n=node, si node n'est pas trouvé renvois (n,n)*)
let rec recup_node_visited visited node=
match visited with
|[]->(node,node)
| (n,p)::rest -> if (n=node) then (n,p) else recup_node_visited rest node;;

(*on met pas la source car elle est implicite c'est forcement la precedent de toutes les precedentes*)
(*A partir de la liste des notes visité (liste au format (node,precedente)) on retrouve le chemin menant a la node d *)
(*si la node d n'est pas dans visited ie elle n'est pas atteinte on renvois []*)
let chemin visited d=
let rec boucle_while liste node_prec = 
    match node_prec with
    |(x,y)->if x=y then liste else (boucle_while ((x,y)::liste) (recup_node_visited visited y) )
in
let (d2,p2)=recup_node_visited visited d in
if d2=p2 then [] else boucle_while [(d2,p2)] (recup_node_visited visited p2)
;;

(*inverse l'ordre des elements d'une liste*)
let rec reverseliste liste acu=
match liste with
|[]->acu
|e::rest->reverseliste rest (e::acu);;

(*renvois l'arc demandé si il existe et un arc fictif sinon*)
let trouve_arc gr ids idd=
match (find_arc gr ids idd) with
  |None->{src=ids ;tgt=idd ;lbl=0}
  |Some arci->{src=arci.src ;tgt=arci.tgt ;lbl=arci.lbl};;
;;

(*renvoi le flot max du graph, qui est la somme des label sortant de la source*)
let flot_max gr s=
let rec boucle liste_arc acu=
  match liste_arc with
  |[]->acu
  |e::rest->boucle rest (acu+e.lbl)
in
boucle (out_arcs gr s) 0
;;

(*floyd*)
(*algo de floyd fulkerson, renvois le graph de flot max entre s et p*)
let floyd gr s p=
  let liste_node=recup_nodes_gr gr in
  let gr2=gmap gr (fun x->x*0) in (*tout le long de l'algo on va travailler avec gr2 qui sera initier avec un debit de 0*)
  let gr_flot=clone_nodes gr2 in
  
  let rec boucle_arc1 liste_arc gr_flot gr=(*arc dans le sens normal (augmentation du debit) avec pour label = capacité max - flot actuel  les graph avec un label a 0 ne sont pas mis dans le graph de flot*)
    match liste_arc with
    |[]->gr_flot
    |{src=ids;tgt=idd;lbl=lbla}::rest->begin
      let valeur=(trouve_arc gr ids idd).lbl - lbla in 
      if (valeur <> 0 ) then (boucle_arc1 rest (add_arc gr_flot ids idd valeur) gr) else (boucle_arc1 rest gr_flot gr)
    end
in
let rec boucle_arc2 liste_arc gr_flot gr=(*arc dans le sens oposé (diminution du debit) avec pour label=-flot actuel, les arc avec un label a 0 ne sont pas mis dans le graph de flot*)
    match liste_arc with
    |[]->gr_flot
    |{src=ids;tgt=idd;lbl=lbla}::rest->
      if lbla <> 0 then (boucle_arc2 rest (add_arc gr_flot idd ids (-lbla)) gr) else (boucle_arc2 rest gr_flot gr)

in
let boucle_arc liste_arc gr_flot gr=(*dans les étapes du graph de flot on reconstruit 2 arc pour chaque arc d'origine un dans le sens normal et un dans l'autre sens*)
  boucle_arc2 liste_arc (boucle_arc1 liste_arc gr_flot gr) gr
in
  let rec boucle_flot gr_flot gr2 liste_node =(*renvois le graph de flot de gr2*)
    match liste_node with
    |[]->gr_flot
    |id::rest->boucle_flot (boucle_arc (out_arcs gr2 id) gr_flot gr) gr2 rest
in
  let rec boucle_parcours parcours valeur gr2 gr_flot=(*applique les changement de flot au graph gr2 le long du parcours*)
    match parcours with
    |[]->gr2
    |(n,p)::rest->begin
      if (trouve_arc gr_flot p n).lbl >0 then boucle_parcours rest valeur (add_arc gr2 p n valeur) gr_flot
      else boucle_parcours rest valeur (add_arc gr2 n p(-valeur) ) gr_flot
    end
  in
  let rec val_min parcours v_min gr_flot=(*recupere la valeur d'augmentation de flot du parcours*)
    match parcours with
    |[]->v_min
    |(n,p)::rest->val_min rest (min v_min (abs (trouve_arc gr_flot p n).lbl) ) gr_flot
in
let rec boucle_while parcours gr2 gr_flot s p=
if parcours <>[] then begin  
    let valeur=val_min parcours max_int gr_flot in
    let gr2=boucle_parcours parcours valeur gr2 gr_flot in
    let gr_flot=boucle_flot (clone_nodes gr2) gr2 liste_node in
    let parcours=reverseliste (chemin (bfs gr_flot s) p) [] in
    boucle_while parcours gr2 gr_flot s p;
  end
else gr2
in
let gr_flot=boucle_flot gr_flot gr2 liste_node in
let parcours= reverseliste (chemin (bfs gr_flot s) p) [] in

boucle_while parcours gr2 gr_flot s p(*l'algo s'execute jusqua ce qu'il n'y ai plus de chemin possible entre la source et le puit sur le graph de flot*)
;;



