open Types

(* ------------------------------------ *)

let array_size = 10105
let number_of_nodes = 10000
                 
let node_size = 5

(* ------------------------------------ *)
(* ------------------------------------ *)

let first (a,b,c) = a
let second (a,b,c) = b
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
let blue = Graphics.rgb 0 160 0
let yellow = Graphics.rgb 255 255 0
let c4 = Graphics.rgb 160 0 0
let green = Graphics.rgb 0 160 0
let c6 = Graphics.rgb 0 0 160
let red = Graphics.rgb 240 0 0
let c8 = Graphics.rgb 0 120 0
let blue = Graphics.rgb 0 0 240
let black = Graphics.rgb 0 0 0

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

let draw_point (x,y) r color = Graphics.set_color color;
                               Graphics.draw_circle x y r;
                               Graphics.fill_circle x y r 

let rec delay n = if n = 0 
                  then ()
                  else (tick (10.0 /. time_factor_constant);
                        delay (n-1))

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

let start =
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

let  paint_initial positions = 
    (* ------- *)
    let paint_susceptible pnt = draw_point (map_point pnt) node_size green in
    for i =0 to 9999 do paint_susceptible positions.(i) done
        
let  paint_initially_infected infected_list node_array positions =
      let paint_infected pnt = draw_point (map_point pnt) node_size red in
      let node_to_point node = positions.(first node_array.(node)) in 
      let infected_points = List.map node_to_point infected_list in
      List.iter paint_infected infected_points
      
let paint_events event_list node_array positions =
    let paint_node pnt color = draw_point (map_point pnt) node_size color in  
    let paint_individual species = match species with 
           ("I",[n]) -> paint_node positions.(first node_array.(int_of_string n)) red
         | ("S",[n]) -> paint_node positions.(first node_array.(int_of_string n)) green
         | ("R",[n]) -> paint_node positions.(first node_array.(int_of_string n)) blue
         |         _ -> ()
   in let paint_event (time,_,right) = 
            draw_time time 800 960;
            List.iter paint_individual right; 
            delay 20;() 
   in List.iter paint_event event_list              

(* --------------------------------------- *)

let rec populate_array lst k node_array = match lst with [] -> () |
                            (x,c)::tail -> node_array.(x) <- (k,x,c);
                                           populate_array tail (k+1) node_array  

let rec populate_array1 lst k node_array = match lst with [] -> () |
                            (x,c)::tail -> node_array.(x) <- x;
                                           populate_array1 tail (k+1) node_array  

let write_note () = Graphics.set_color black;
                    Graphics.moveto 140 10;
                    Graphics.draw_string "The degree distributions of the nodes degrease from the center outwards."


let draw_results positions node_degree_dis initially_infected event_list = 
                              let length = (List.length node_degree_dis)+1 in
                              let node_array = Array.make length (0,0,0) in
                              let max_dd = second_of_pair (List.hd node_degree_dis) in
                              let _ = populate_array node_degree_dis 1 node_array in
                              List.iter print_count node_degree_dis; 
                              start;
                              write_note ();
                              (* for i =0 to 9999 do write node_array.(i) done; *)
                              paint_initial positions; 
                              paint_initially_infected initially_infected node_array positions;
                              delay 8000;
                              paint_events event_list node_array positions; 
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
                 let node_degree_dis = get_counts reac_list max in 
                 (* ----------------------------------- *)
                 let initially_infected = get_initially_infected reac_list [] in
                 (* ----------------------------------- *)
                 let positions = Array.make array_size (0,0)  in  
                 let _ = nums 1 (0,0) true (0,1) 1 positions in
                 (* List.iter print_count node_degree_dis;  *)
                 print_string "\n";
                 draw_results  positions node_degree_dis  initially_infected  reac_list


(* ########################################################################################## *)
(* ########################################################################################## *)