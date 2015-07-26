type 'a t = 'a list ref

exception Empty

let create () = ref []

let add x w =
  try
    List.iter (fun x' -> if x = x' then raise Exit) !w;
    (* We did not find x, so add it. *)
    w := x :: !w
  with Exit -> ()

let push = add

let add_list l w =
  List.iter (fun e -> add e w) l

let take w = match !w with
  | h::tl -> w := tl; h
  | [] -> raise Empty

let filter f w =
  w := List.filter f !w

let pop = take

let peek w = match !w with
  | h::tl -> h
  | [] -> raise Empty

let top = peek

let all w = !w

let clear w = w := []

let is_empty w = !w = []

let length w = List.length !w

let iter f w = List.iter f !w

let fold f a w = List.fold_left f a !w
