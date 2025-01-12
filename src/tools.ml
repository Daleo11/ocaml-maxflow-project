open Graph

(*on lui donne un graph il renvoi les nodes sans les arcs*)
let clone_nodes gr= 
  n_fold gr new_node empty_graph
;;


let add_arc g id1 id2 n=
  match (find_arc g id1 id2) with
  |None->new_arc g {src=id1 ;tgt=id2 ;lbl=n}
  |Some arci-> new_arc g {src=id1 ;tgt=id2 ;lbl=arci.lbl+n};;

let gmap gr f=
  let transfo g arc=new_arc g {src = arc.src ;tgt=arc.tgt; lbl=f arc.lbl }in
  e_fold gr transfo (clone_nodes gr)
;;

