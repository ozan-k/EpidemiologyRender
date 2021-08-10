open Types

let write_species (name,pl) =
             let rec w_par_list l = match l with
                       [] -> ()
                   | h::t -> print_string (h ^ " ");
                             w_par_list t in
                             print_string ( name ^ " ");
                             w_par_list pl

let rec write_spec_list l = match l with
              [] -> ()
          | h::t -> write_species  h;
                    print_string " ";
                    write_spec_list t


let rec write_reaction_list (l:Types.reaction list) = match l with
            [] -> ()
        |  (time,left,right)::t ->
                            print_string (( time ) ^ " " );
                            write_spec_list left;
                            print_string " --> ";
                            write_spec_list right;
                            print_string ( " \n" );
                            write_reaction_list t

(* Implements a delay of n seconds. *)
let tick n =
      let t = Sys.time() in
      while (Sys.time ()) < t +. n do
        print_string "" done

(* Filters out all the processes except
   those given in l and all the parameters
   except first n parameters in the reaction list *)
let rec filter_parameters n param_list =
        match param_list with
            [] -> []
        | h::t -> if n < 1 then [] else
                  if n > 1 then h::(filter_parameters (n-1) t)
                  else [h]


let rec filter_species_list lst num species_list =
       match species_list with
           [] -> []
        | (name,parameters)::t -> if List.mem name lst
                      then (name,(filter_parameters num parameters))::(filter_species_list lst num t)
                      else (filter_species_list lst num t)

let filter_reaction lst num (time,left,right) =
           (time, (filter_species_list lst num left),(filter_species_list lst num right))


let rec filter_reaction_list lst num reactions = match reactions with
         [] -> []
       | h::t -> (filter_reaction lst num h)::(filter_reaction_list lst num t)

(* ########################################################################################## *)
(* ########################################################################################## *)

type proc = (string * ( float * float ) * float )

let c1 = Graphics.rgb 80 0 0
let c2 = Graphics.rgb 0 80 0
let c3 = Graphics.rgb 0 0 80
let yellow = Graphics.rgb 255 255 0
let c4 = Graphics.rgb 160 0 0
let c5 = Graphics.rgb 0 160 0
let c6 = Graphics.rgb 0 0 160
let c7 = Graphics.rgb 240 0 0
let c8 = Graphics.rgb 0 240 0
let c9 = Graphics.rgb 0 0 240

let position_x = 1600(*600*)
let position_y = 1600(*1210*)

let rc (*resolution_constant*) = 1100.0 (*200*)
let rcr (*resolution_constant for radius of the circle *) =  2.0  (* 8 *)
let time_factor_constant (*the factor with which the event duration is multiplied during rendering*) = 3000.0
let dimension = 3

(* ########################################## *)

(* http://en.wikipedia.org/wiki/3D_projection *)

(* The position of a point c representing the camera. *)
let c_x = 6.0   (*0.0 *)
let c_y = 22.0   (*6.0 *)
let c_z = 6.0  (*-6.0*)

(* The orientation of the camera. *)
let theta_x = 1.57079
let theta_y = 0.0 (*3.14*)
let theta_z = 0.0

(* The viewer's position relative to the display surface. *)
let e_x = 3.0 (*0.0  *)
let e_y = 3.0 (*2.0  *)
let e_z = 3.0 (*-4.0 *)

(* The position of a point c representing the camera. *)
(*
let c_x = -0.2
let c_y = 0.0
let c_z = -0.1
*)
(* The orientation of the camera. *)
(*
let theta_x = 0.1
let theta_y = 1.2
let theta_z = 0.1
*)

(* The viewer's position relative to the display surface. *)
(*
let e_x = -0.35
let e_y = -0.1
let e_z = -2.7
*)

let point x y z =
     let d_x  = (cos theta_y) *. ( (sin theta_z) *. (y -. c_y) +. (cos theta_z) *. (x -. c_x))  -. (sin theta_y) *. (z -. c_z)

  in let d_y = (sin theta_x) *. ( (cos theta_y) *. (z -. c_z) +. (sin theta_y) *. ((sin theta_z) *. (y -. c_y) +. (cos theta_z) *. (x -. c_x)))
                    +. (cos theta_x) *. ((cos theta_z) *. (y -. c_y) -. (sin theta_z) *. (x -. c_x))

  in let d_z = (cos theta_x) *. ( (cos theta_y) *. (z -. c_z) +. (sin theta_y) *. ((sin theta_z) *. (y -. c_y) +. (cos theta_z) *. (x -. c_x)))
                    -. (sin theta_x) *. ((cos theta_z) *. (y -. c_y) -. (sin theta_z) *. (x -. c_x))

  in let b_x = (d_x -. e_x) *. (e_z /. d_z)

  in let b_y = (d_y -. e_y) *. (e_z /. d_z)

  in (b_x,b_y)

(* ################################# *)

let color_map_for_process p = match p with
                     "Cb" -> c4
                   | "Cx" -> c4
                   | "Alb" -> c4
                   | "Al" -> c4
                   | "A" -> c1
                   | "Ab" -> c4
                   | "El" -> yellow
                   | "Er" -> c1
                   | "Ell" -> c2
                   | "Err" -> c2
                   | "Arb" -> c2
                   | "Arbb" -> c2
                   | "IgG" -> c3
                   | "FcR" -> c3
                   | "S" -> c4
                   | "T" -> c5
                   | "U" -> c6
                   | "X" -> c7
                   | "Y" -> c8
                   | "Z" -> c9
                   |   _ -> ( Graphics.rgb 0 0 0 )

let rec draw_process_list (pl: proc list) = match pl with
           [] -> ()
        | (p,(x,y),r)::t -> (* change color for p;*)
                Graphics.set_color (color_map_for_process p);
                (Graphics.draw_circle (position_x + (int_of_float (x *. rc )))
				                         (position_y + (int_of_float (y *. rc )))
										                    (int_of_float (rcr*.r));
                 Graphics.fill_circle (position_x + (int_of_float (x *. rc )))
				                         (position_y + (int_of_float (y *. rc )))
										                    (int_of_float (rcr*.r)));
                        draw_process_list t

let rec erase_process_list (pl: proc list) = match pl with
           [] -> ()
        | (p,(x,y),r)::t -> Graphics.draw_circle
		                           (position_x + (int_of_float (x *. rc)))
								         (position_y + (int_of_float (y *. rc)))
										         (int_of_float (rcr*.r));
                        Graphics.fill_circle
						           (position_x + (int_of_float (x *. rc)))
						                 (position_y + (int_of_float (y *. rc)))
										         (int_of_float (rcr*.r));
                        erase_process_list t

(* ####### *)

let compute_radius mass = ( mass (*/. 4.18879020479*)) ** 0.333333333

let rec convert_species2D species_lst = match species_lst with
                        [] -> []
           | (name,[x;y;r])::t -> (name,
                                ((float_of_string x),
                                   (float_of_string y)),float_of_string r)::(convert_species2D t)
           | (_,_)::t ->   (convert_species2D t)

let rec convert_species3D species_lst = match species_lst with
                        [] -> []
           | (name,[x;y;z;r])::t -> (name, point ((float_of_string y) +. 0.5) ((float_of_string x) +. 0.5) ((float_of_string z) -. 0.0),compute_radius (float_of_string r))::(convert_species3D t)
           | (_,_)::t ->   (convert_species3D t)

let convert_species species_lst = if dimension = 3
                                  then convert_species3D species_lst
                                  else convert_species2D species_lst

(* #######*)
let draw_event (time,left, right) =
                                   tick ((float_of_string time) /. time_factor_constant);
                                   Graphics.set_color ( Graphics.rgb 255 255 255 );
                                   (*erase_process_list ( convert_species left);*)
                                    draw_process_list ( convert_species right)

let rec draw_all_events event_list = match event_list with
                     [] -> ()
                   | h::t -> draw_event h; draw_all_events t


(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

let start =
 Graphics.open_graph " ";
 Graphics.resize_window 1000 1000;
 Graphics.clear_graph ();
 print_string "...."


let my_moveto x y z =
         let (x1,y1) = point x y z
      in Graphics.moveto (position_x + (int_of_float (x1 *. rc))) (position_y + (int_of_float (y1 *. rc)))

let my_lineto x y z =
        let (x1,y1) = point x y z
      in Graphics.lineto (position_x + (int_of_float (x1 *. rc))) (position_y + (int_of_float (y1 *. rc)))


let draw_cube () =
   Graphics.set_color ( Graphics.rgb 160 0 0 );
   my_moveto  0.0 0.0 0.0 ;
   my_lineto  0.0 0.0 1.0 ;
   my_lineto  0.0 3.0 3.0 ;
   my_lineto  0.0 2.0 0.0 ;
   my_lineto  0.0 0.0 0.0 ; (* black x = 0 *)

   Graphics.set_color ( Graphics.rgb 0 160 0 );
   my_moveto  0.0 0.0 0.0 ;
   my_lineto  0.0 0.0 2.0 ;
   my_lineto  2.0 0.0 2.0 ;
   my_lineto  2.0 0.0 0.0 ;
   (*my_moveto  0.0 0.0 1.0 ;
   my_lineto  2.0 0.0 1.0 ;
   my_moveto  1.0 0.0 0.0 ;
   my_lineto  1.0 0.0 2.0 ; *)(* red y = 1 *)

   Graphics.set_color ( Graphics.rgb 0 0 160 );
   my_moveto   0.0 0.0 0.0;
   my_lineto   0.0 2.0 0.0;
   my_lineto   2.0 2.0 0.0;
   my_lineto   2.0 0.0 0.0;
   my_lineto   0.0 0.0 0.0;  (*blue z = 0*)
   my_moveto   1.0 0.0 0.0;
   my_lineto   1.0 2.0 0.0;
   my_moveto   0.5 0.0 0.0;
   my_lineto   0.5 2.0 0.0;
   my_moveto   1.5 0.0 0.0;
   my_lineto   1.5 2.0 0.0;

   my_moveto   0.0 0.5 0.0;
   my_lineto   2.0 0.5 0.0;
   my_moveto   0.0 1.5 0.0;
   my_lineto   2.0 1.5 0.0;


   my_moveto   0.0 1.0 0.0;
   my_lineto   2.0 1.0 0.0;


   Graphics.set_color ( Graphics.rgb 160 0 0 );
   my_moveto   0.0 0.0 0.0;
   my_lineto   3.0 0.0 0.0;
   Graphics.set_color ( Graphics.rgb 0 160 0 );
   my_moveto   0.0 0.0 0.0;
   my_lineto   0.0 3.0 0.0;
   Graphics.set_color ( Graphics.rgb 0 0 160 );
   my_moveto   0.0 0.0 0.0;
   my_lineto   0.0 0.0 3.0


let rec delay event_list n = match event_list with
                     [] -> ()
                   | (time,_left,_right) ::t -> if n = 0
                                                then ()
                                                else (tick ((float_of_string time) /. time_factor_constant);
                                                      delay t (n-1))


let draw_results event_list =
                              start;
                              draw_cube ();
                              delay event_list 0;
							                draw_all_events event_list;
                              Graphics.close_graph


(* ########################################################################################## *)
(* ########################################################################################## *)

let _ =
  let ic:in_channel = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ic in
  let reac_list = Parser.reactions Lexer.token lexbuf in
            (* write_reaction_list *)
		   draw_results (filter_reaction_list ["Cb";"Cx";"Ab";"Alb";"Alb1";"Alb2"] (dimension+1) reac_list)


(* ########################################################################################## *)
(* ########################################################################################## *)

(* trans <  ml3 *)
