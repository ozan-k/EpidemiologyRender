open Types

(* ------------------------------------ *)

let array_size = 10105
let number_of_nodes = 10000
                 
let node_size = 5

let color_scaling_function x = x 

(* ------------------------------------ *)
(* ------------------------------------ *)

let first (a,b,c) = a
let second (a,b,c) = b
let third (a,b,c) = c
let first_of_pair (a,b) = a
let second_of_pair (a,b) = b

let turn vector = match vector with 
           |(0,1) -> (1,0)   
           |(1,0) -> (0,-1)  
           |(0,-1) -> (-1,0)  
           |(-1,0) -> (0,1)  
           | _ -> (1,1)

let rec instantiate_n_pos n (x,y) (vx,vy) counter positions = 
                      if n = 0 
                      then (x,y) 
                      else (positions.(counter)  <- (x+vx,y+vy);
                            instantiate_n_pos (n-1) (x+vx,y+vy) (vx,vy) (counter+1) positions )

let rec nums n init semafor vector counter positions = 
                         if n =  101 
                         then positions.(0) <- (0,0)
                         else let next = instantiate_n_pos n init vector counter positions
                              in if semafor 
                                 then nums n next (not semafor) (turn vector)   (counter+n) positions
                                 else nums (n+1) next (not semafor) (turn vector)  (counter+n) positions

(* ------------------------------------ *)
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
let yellow = Graphics.rgb 255 255 0
let c4 = Graphics.rgb 160 0 0
let c6 = Graphics.rgb 0 0 160
let c8 = Graphics.rgb 0 120 0
let red = Graphics.rgb 240 0 0
let green = Graphics.rgb 0 160 0
let blue = Graphics.rgb 0 0 240
let black = Graphics.rgb 0 0 0
let white = Graphics.rgb 255 255 255

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


let my_moveto x y = Graphics.moveto x y
 
let my_lineto (x1,y1) (x2,y2) color = Graphics.set_color color;
                                      Graphics.moveto x1 y1; 
                                      Graphics.lineto x2 y2

let draw_point (x,y) r color = Graphics.set_color white;
                               Graphics.draw_circle x y r;
                               Graphics.fill_circle x y r;
                               Graphics.set_color color;
                               Graphics.draw_circle x y r;
                               Graphics.fill_circle x y r 

let rec delay n = if n = 0 
                  then ()
                  else (tick (10.0 /. time_factor_constant);
                        delay (n-1))

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

let start () =
        Graphics.open_graph " ";
        Graphics.resize_window 1000 1000;
        Graphics.clear_graph ();
        Graphics.set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
        draw_point (70,975) 5 green;
        draw_point (300,975) 5 red;
        draw_point (530,975) 5 blue;
        Graphics.set_color black;
        Graphics.moveto 85 965; 
        Graphics.draw_string "Susceptible";
        Graphics.moveto 315 965; 
        Graphics.draw_string "Infected";
        Graphics.moveto 545 965; 
        Graphics.draw_string "Recovered/Removed";
        print_string "...."
                         
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
                         
let map_point (a,b) = (500+9*a,500+9*b)

let print_single a = print_string ((string_of_int a) ^ " ") 

let print_pair (a,b) = print_string ("(" ^ (string_of_int a) ^ "," ^ 
                                        (string_of_int b)  ^") ") 

let print_triple (a,b,c) = print_string ("(" ^ (string_of_int a) ^ "," ^
                                               (string_of_int b) ^ "," ^  
                                               (string_of_int c)  ^")\n") 

let print_singles lst = List.iter print_single lst; print_string "\n" 
let print_pairs lst = List.iter print_pair lst; print_string "\n"                                          

(* ------------------------------------ *)
(* ((log (float_of_int dd)) * dd_factor) *)

let color_of dd color max_dd = 
  (* "color_scaling_function" could be used here, defined above *)
    let base  = 252 - ((max_dd  * 2) / 10) in 
     let dd_effect =   base + (( dd  * 2) / 10) in
     let cof clr = match clr with 
        1 -> red
        (* Graphics.rgb 180  0 0 *)
      | 2 -> green
      (* Graphics.rgb 0 180 0 *)
      | 3 -> blue 
      (* Graphics.rgb 0 0 180 *)
      | _ -> Graphics.rgb 0 0 (dd_effect) 
    in cof color

let  paint_initial positions node_array dd_factor max_dd length = 
    (* ------- *)
    let paint_susceptible pnt dd = draw_point (map_point pnt) node_size (color_of dd 2 max_dd) in
    for i =0 to (length-1)  do 
    (* print_string ((string_of_int i) ^ " "); *)
    paint_susceptible positions.(first node_array.(i)) (third node_array.(i)) done
        
let  paint_initially_infected infected_list node_array positions dd_factor max_dd =
      let paint_infected pnt = draw_point (map_point (first_of_pair pnt)) node_size (color_of (second_of_pair pnt) 1 max_dd) in
      let node_to_point node = (positions.(first node_array.(node)),third node_array.(node)) in 
      let infected_points = List.map node_to_point infected_list in
      List.iter paint_infected infected_points
      
let paint_events event_list node_array positions dd_factor max_dd =
    let paint_node pnt color = draw_point (map_point pnt) node_size color in  
    let paint_individual species = match species with 
           ("I",[n]) -> paint_node positions.(first node_array.(int_of_string n)) (Graphics.rgb 255 255 255 );
                        paint_node positions.(first node_array.(int_of_string n)) (color_of (third node_array.(int_of_string n)) 1 max_dd)
         | ("S",[n]) -> paint_node positions.(first node_array.(int_of_string n)) (Graphics.rgb 255 255 255 );
                        paint_node positions.(first node_array.(int_of_string n)) (color_of (third node_array.(int_of_string n)) 2 max_dd)
         | ("R",[n]) -> paint_node positions.(first node_array.(int_of_string n)) (Graphics.rgb 255 255 255 );
                        paint_node positions.(first node_array.(int_of_string n)) (color_of (third node_array.(int_of_string n)) 3 max_dd)
         |         _ -> ()
   in let paint_event (time,_,right) = 
            draw_time time 800 960;
            List.iter paint_individual right; 
            delay 35;() 
   in List.iter paint_event event_list              

(* --------------------------------------- *)

let rec populate_array lst k node_array = match lst with [] -> () |
                            (x,c)::tail -> node_array.(x) <- (k,x,c);
                                           populate_array tail (k+1) node_array  

let rec populate_array1 lst k node_array = match lst with [] -> () |
                            (x,c)::tail -> node_array.(x) <- x;
                                           populate_array1 tail (k+1) node_array  

let write_note alpha beta = 
                    Graphics.set_color black;
                    Graphics.moveto 80 10;
                    Graphics.draw_string ( "Alpha: " ^ alpha ^ ", Beta: " ^ beta ^ " ~ The degree distribution degreases from the center outwards.")


let draw_results alpha beta positions node_degree_dis initially_infected event_list = 
                              let length = (List.length node_degree_dis)+1 in
                              let node_array = Array.make length (0,0,0) in
                              let max_dd = second_of_pair (List.hd node_degree_dis) in
                              let dd_factor = 220.0 /. (color_scaling_function (float_of_int max_dd)) in
                              let _ = populate_array node_degree_dis 0 node_array in
                              (* List.iter print_count node_degree_dis;  *)
                              (* for i = 0 to 9999  do print_pair positions.(i) done; *)
                              (* print_string (string_of_int length); *)
                              start ();
                              write_note alpha beta;
                              paint_initial positions node_array dd_factor max_dd length; 
                              paint_initially_infected initially_infected node_array positions dd_factor max_dd; 
                              delay 20000;
                              paint_events event_list node_array positions dd_factor max_dd; 
                              delay 10000;
                              Graphics.close_graph () 
                                                            
(* ########################################################################################## *)
(* ########################################################################################## *)

(* Print  reaction instances *)
let print_cr c = print_string (c ^ " ")
let print_ind (x,yl) = print_string (x ^ "~");
                       List.iter print_cr yl  
let print_reac (time,l,r) = print_string (time ^ " @ "); 
                            List.iter print_ind  l; 
                            print_string " -> ";
                            List.iter print_ind  r; 
                            print_string ("\n")


 
(* Print a node ID *)
let print_node x = print_string ((string_of_int x) ^ " ") 


let print_node_dd (a,b) = print_string ((string_of_int a) ^ " ~> " ^ (string_of_int b) ^ " | ")

let get_parameters str = 
  let i = (String.index_from  str 30 'b') + 1 in  
  let beta = String.sub str i 5 in 
  let j = (String.index_from  str 35 'a') + 1 in  
  let alpha_ = String.sub str j 4 in 
  let alpha = if alpha_ = "2.0," then "2.0" else alpha_ in 
  (alpha,beta)

let _ =
  let file_name = Sys.argv.(1) in
  let ic:in_channel = open_in file_name in
  let (alpha,beta) = get_parameters file_name in 
  let _ = print_string ("Alpha: " ^ alpha ^ ", Beta: " ^ beta ^ "\n") in 
  let lexbuf = Lexing.from_channel ic in
  let reac_list = Parser.reactions Lexer.token lexbuf in
  (* let _ = List.iter print_reac reac_list; print_string "\n" in *)
  let nodes = get_nodes reac_list in
  (* let _ = List.iter print_node nodes;print_string "\n" in *)
  let max = get_max nodes 0 in
  (* let _ = print_string ((string_of_int max) ^"\n")  in                             *)
  let node_degree_dis = get_counts reac_list max in 
  (* let _ = List.iter print_node_dd node_degree_dis in *)
  (* ----------------------------------- *)
  let initially_infected = get_initially_infected reac_list [] in
  (* ----------------------------------- *)
  let positions = Array.make array_size (0,0)  in  
  let _ = nums 1 (0,0) true (0,1) 1 positions in
  (* List.iter print_count node_degree_dis;   *)
  draw_results  alpha beta positions node_degree_dis  initially_infected  reac_list;
  print_string "\n";
  ();   
    
  

(* ########################################################################################## *)
(* ########################################################################################## *)
(* 
32.6327153881
max dd: 847
*)

