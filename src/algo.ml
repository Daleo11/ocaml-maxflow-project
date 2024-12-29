open Graph



(*parcours en largeur, graph source puit*)

let bfs gr s p=
let file =[s] in 
  n_fold gr (fun e->e::[] )
;;

(*floyd*)