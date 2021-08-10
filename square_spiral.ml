(* #!/Users/ozan/.opam/4.07.1/bin/ocaml  *)

let rec print_n_dash1 n = if n = 0 then () else (print_string "-";print_n_dash1 (n-1))

let rec nums1 n semafor = if n = 10 
                         then print_string "\n"
                         else (print_n_dash1 n;print_string "\n";
                              if semafor 
                              then nums1 n (not semafor)
                              else nums1 (n+1) (not semafor))       

(* let a1 = nums1 1 true *)

(* -------------------------------- *)

let turn2 vector = match vector with 
           |"^" -> ">"  
           |">" -> "v" 
           |"v" -> "<" 
           |"<" -> "^" 
           | _ -> ""

let rec print_n_dash2 n vector = if n = 0 then () else (print_string vector; print_n_dash2 (n-1) vector)

let rec nums2 n semafor vector = if n = 10 
                         then print_string "\n"
                         else (print_n_dash2 n vector;print_string "\n";
                              if semafor 
                              then nums2 n (not semafor) (turn2 vector)
                              else nums2 (n+1) (not semafor) (turn2 vector))       

(* let a2 = nums2 1 true "^" *)

(* -------------------------------- *)

let print_vector3 (x,y) = print_string ("(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") ") 

let turn3 vector = match vector with 
           |(0,1) -> (1,0)   
           |(1,0) -> (0,-1)  
           |(0,-1) -> (-1,0)  
           |(-1,0) -> (0,1)  
           | _ -> (1,1)

let rec print_n_dash3 n (x,y) (vx,vy) = if n = 0 
                                       then (x,y) 
                                       else (print_vector3 (x+vx,y+vy); 
                                             print_n_dash3 (n-1) (x+vx,y+vy) (vx,vy))

let rec nums3 n init semafor vector = 
                         if n = 10 
                         then print_string "\n"
                         else let next = print_n_dash3 n init vector
                              in print_string "\n";
                                 if semafor 
                                 then nums3 n next (not semafor) (turn3 vector)
                                 else nums3 (n+1) next (not semafor) (turn3 vector)      

(* let a = nums3 1 (0,0) true (0,1)  *)

(* -------------------------------- *)

let print_vector4 (x,y) = print_string ("(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") ") 

let turn4 vector = match vector with 
           |(0,1) -> (1,0)   
           |(1,0) -> (0,-1)  
           |(0,-1) -> (-1,0)  
           |(-1,0) -> (0,1)  
           | _ -> (1,1)

let rec print_n_dash4 n (x,y) (vx,vy) counter = if n = 0 
                                       then (x,y) 
                                       else (print_string ((string_of_int counter) ^ "<-" ^ ""); 
                                             print_vector4 (x+vx,y+vy);
                                             print_string " ~ "; 
                                             print_n_dash4 (n-1) (x+vx,y+vy) (vx,vy) (counter+1))

let rec nums4 n init semafor vector counter = 
                         if n = 101 
                         then print_string "\n"
                         else let next = print_n_dash4 n init vector counter
                              in print_string "\n";
                                 if semafor 
                                 then nums4 n next (not semafor) (turn4 vector)   (counter+n)
                                 else nums4 (n+1) next (not semafor) (turn4 vector)  (counter+n)     

(* let a = nums 1 (0,0) true (0,1) 1  *)

(* -------------------------------- *)

let size = 10105

let print_vector (x,y) = print_string ("(" ^ (string_of_int x) ^ "," ^ (string_of_int y) ^ ") ") 

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
                         then print_string ""
                         else let next = instantiate_n_pos n init vector counter positions
                              in if semafor 
                                 then nums n next (not semafor) (turn vector)   (counter+n) positions
                                 else nums (n+1) next (not semafor) (turn vector)  (counter+n) positions     

let pos_array = 
          let positions = Array.make size (0,0)    
          in let _ = nums 1 (0,0) true (0,1) 1 positions 
          in positions  

let _ = pos_array