open Types

(* ------------------------------------ *)

let number_of_nodes = 10000

let increment = 485.0 /. (float_of_int number_of_nodes)
                 
(* ------------------------------------ *)

let f_point n =
    let factor = if              n < 50.     then  1.0
            else if n >= 50. && n < 100.     then  1.0
            else if n >= 100. && n < 200.    then  1.0
            else if n >= 200. && n < 300.    then  1.0
            else if n >= 300. && n < 400.    then  1.0
            else if n >= 400. && n < 500.    then  1.0
            else if n >= 500. && n < 1000.   then  1.0
            else if n >= 2000. && n < 3000.  then  1.0
            else if n >= 3000. && n < 4000.  then  1.0
            else if n >= 4000. && n < 5000.  then  1.0
            else if n >= 5000. && n < 6000.  then  1.0
            else if n >= 6000. && n < 7000.  then  1.0
            else if n >= 7000. && n < 8000.  then  1.0
            else if n >= 8000. && n < 9000.  then  1.0
            else if n >= 9000. && n < 10000. then  1.0
            else                                   1.0
            in increment *. n *.factor 

let rec f_line n = if n >= (float_of_int number_of_nodes) 
                   then [] 
                   else (f_point n)::f_line (n+.1.) 

(* ------------------------------------ *)

let print_node node = print_string ((string_of_int node) ^ ",")
let print_count (a,b) = print_string ((string_of_int a) ^ "," ^ (string_of_int b) ^ "\n")
let rec get_max lst n = match lst with [] -> n |
       h::tail ->  get_max tail (if h > n then h else n)

(* ------------------------------------ *)

let rec remove_int_dup lst = match lst with 
                    [] -> [] |
               h::tail -> if List.mem h tail 
                          then remove_int_dup tail
                          else (int_of_string h)::remove_int_dup tail

let get_nodes reac_list =  
         let rec nodes_of lst = match lst with 
                          [] -> [] |
                 (a,b)::tail -> b @ nodes_of tail 
      in let rec g_nodes lst = match lst with 
                          [] -> [] |
     (time,left,right)::tail -> (nodes_of left) @ (nodes_of right) @ (g_nodes tail) 
      in List.sort compare (remove_int_dup (g_nodes reac_list))

(* ------------------------------------ *)
            
let get_counts reac_list max_node_id = 
     let my_compare (a,b) (c,d) = if b > d then -1 else 1 in 
     let a = Array.make (max_node_id+1) 0 in
     let update (_time,left,_right) = 
        match left with 
        [(s1,[n1]);(s2,[n2])] -> 
               a.(int_of_string n1) <- a.(int_of_string n1) + 1;
               a.(int_of_string n2) <- a.(int_of_string n2) + 1
       | _ -> () in
     let _  = List.iter update reac_list in 
     let rec array_to_list n = if n = (max_node_id+1) 
                               then []
                               else (n,a.(n))::array_to_list (n+1) in
     List.sort my_compare (array_to_list 1)                          

(* ------------------------------------ *)

let rec get_initially_infected reac_list aux =  match reac_list with 
                                          [] -> []
           | (_,[("I",[n1]);("S",[n2])],_)::tail -> 
             let n1s = int_of_string n1 in 
             let n2s = int_of_string n2 in
                        if List.mem n1s aux
                        then get_initially_infected tail (n1s::n2s::aux)
                        else n1s::(get_initially_infected tail (n1s::n2s::aux))
           | (_,[("S",[n1]);("I",[n2])],_)::tail -> 
             let n2s = int_of_string n2 in
             let n1s = int_of_string n1 in 
                        if List.mem n2s aux
                        then get_initially_infected tail (n1s::n2s::aux) 
                        else n2s::(get_initially_infected tail (n1s::n2s::aux))
           | (_,[("I",[n1])],_)::tail -> 
                        let n1s = int_of_string n1 in 
                                   if List.mem n1s aux
                                   then get_initially_infected tail (n1s::aux) 
                                   else n1s::(get_initially_infected tail (n1s::aux))  
           | _::tail -> get_initially_infected tail aux                                                                
             
let rec get_ii_indice_main node counts k = match counts with 
              [] -> failwith "Does not happen" |
              (n,c)::tail -> if n = node 
                             then f_point (float_of_int k)
                             else get_ii_indice_main node tail (k+1)

(* ------------------------------------ *)

(* Implements a delay of n seconds. *)
let tick n =
      let t = Sys.time() in
      while (Sys.time ()) < t +. n do
        print_string "" done

(* ########################################################################################## *)
(* ########################################################################################## *)

type proc = ( float * float  * float )
let white = Graphics.rgb 255 255 255
let c1 = Graphics.rgb 80 0 0
let c2 = Graphics.rgb 0 80 0
let c3 = Graphics.rgb 0 0 80
let blue = Graphics.rgb 0 160 0
let yellow = Graphics.rgb 255 255 0
let c4 = Graphics.rgb 160 0 0
let green = Graphics.rgb 0 160 0
let c6 = Graphics.rgb 0 0 160
let red = Graphics.rgb 240 0 0
let c8 = Graphics.rgb 0 240 0
let blue = Graphics.rgb 0 0 240
let black = Graphics.rgb 0 0 0

(* let position_x = 1600(*600*)
let position_y = 1600(*1210*)

let rc (*resolution_constant*) = 1100.0 (*200*)
let rcr (*resolution_constant for radius of the circle *) =  2.0  (* 8 *)
let dimension = 3 *)
let time_factor_constant (*the factor with which the event duration is multiplied during rendering*) = 100000.0

(* ################################# *)

let draw_time t x y =
  Graphics.set_color white;
  Graphics.draw_rect x y 180 25; 
  Graphics.fill_rect x y 180 25;
  Graphics.set_color black;
  Graphics.moveto (x+5) (y+1); 
  Graphics.draw_string t
    
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

let start =
 Graphics.open_graph " ";
 Graphics.resize_window 1000 1000;
 Graphics.clear_graph ();
 Graphics.set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
 print_string "...."

(* #######*)

let my_moveto x y = Graphics.moveto x y
 
let my_lineto (x1,y1) (x2,y2) color = Graphics.set_color color;
                                      Graphics.moveto x1 y1; 
                                      Graphics.lineto x2 y2

let draw_point x y r color = Graphics.set_color color;
                             Graphics.draw_circle x y r;
                             Graphics.fill_circle x y r 

let rec delay n = if n = 0 
                  then ()
                  else (tick (10.0 /. time_factor_constant);
                        delay (n-1))

(* --------------------------------------- *)

let map_point f = (500 + (int_of_float (f*.(sin f))),500 +(int_of_float(f*.(cos f))))                       

let  paint_initial pairs = 
    let paint_pair (a,b) = draw_point a b 2 green in
    List.iter paint_pair pairs

let  paint_initially_infected points =
      let point_pairs = List.map map_point points in 
      let paint_pair (a,b) = draw_point a b 2 red in
      List.iter paint_pair point_pairs
           
let paint_events event_list arr=
    let paint_pair (a,b) color = draw_point a b 2 color in  
    let paint_individual species = match species with 
           ("I",[n]) -> paint_pair (map_point (f_point (float_of_string n))) red
         | ("S",[n]) -> paint_pair (map_point (f_point (float_of_string n))) green
         | ("R",[n]) -> paint_pair (map_point (f_point (float_of_string n))) blue
         |         _ -> ()
   in let paint_event (time,_,right) = 
            draw_time time 50 900;
            List.iter paint_individual right; 
            delay 20;() 
   in List.iter paint_event event_list              

(* --------------------------------------- *)

let draw_results counts event_list initial_pairs initially_infected = 
                              let length = (List.length counts)+1 in
                              let arr = Array.make length 0 in
                              let rec populate_array lst k = match lst with [] -> () |
                                       (x,c)::tail -> arr.(x) <- k;
                                       populate_array tail (k+1)  in 
                              (* List.iter print_count counts; *)
                              populate_array counts 1;               
                              start;
                              (* my_lineto (0,0) (500,500) black;
                              my_lineto (500,500) (1000,0) black;
                              my_lineto (500,500) (0,1000) black;
                              my_lineto (500,500) (1000,1000) black; 
                              draw_point 500 500 5 c7; *)
                              paint_initial initial_pairs; 
                              paint_initially_infected initially_infected;
                              delay 8000;
                              paint_events event_list arr;
                              delay 10000;
                              Graphics.close_graph ()
                                                            
(* ########################################################################################## *)
(* ########################################################################################## *)

let _ =
  let ic:in_channel = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ic in
  let reac_list = Parser.reactions Lexer.token lexbuf in
                 let nodes = get_nodes reac_list in
                 let max = get_max nodes 0 in
                 let points = f_line 1.0 in
                 let pairs = List.map map_point points in
                 let counts = get_counts reac_list max in 
                 let initially_infected = get_initially_infected reac_list [] in
                 let get_ii_indice node = get_ii_indice_main node counts 1 in
                 let initially_infected_indices = List.map get_ii_indice initially_infected in 
                 (* List.iter print_node nodes;
                  print_string "\n\n";
                  print_string (string_of_int (List.length nodes)); *)
                 (* List.iter print_count counts; *)
                  print_string "\n";
                  (* List.iter  print_node initially_infected;
                  print_string "\n\n";
                  print_string (string_of_int (List.length initially_infected_indices)); *)
                  draw_results counts reac_list pairs initially_infected_indices 

(* ########################################################################################## *)
(* ########################################################################################## *)