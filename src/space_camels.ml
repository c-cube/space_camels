
open Notty
open Notty_lwt

open Lwt.Infix

type cell =
  | Alien
  | Player
  | Shot
  | Empty

type dir =
  | Left
  | Right
  | Down of dir (* next dir *)

type victory_state =
  | Won
  | Lost
  | Struggling

(* x,y -> state *)
type state = {
  wx: int;
  wy: int;
  mutable p_x: int; (* position of player *)
  mutable p_y: int;
  mutable count: int; (* total number of frames *)
  mutable victory: victory_state;
  mutable dir: dir; (* direction of alien movement *)
  cells: cell array array array; (* double buffering *)
}

let mk_state wx wy =
  let p_x = wx/2 in
  let p_y = 3 in
  let cells0 =
    Array.init wx
      (fun x ->
        Array.init wy
          (fun y -> match x,y with
            | _ when x=p_x && y=p_y -> Player
            | x, _ when x>5 && x+5<wx && x mod 2 = 0
                && (y+10=wy||y+12=wy) -> Alien
            | _ -> Empty))
  in
  let cells1 = Array.init wx (fun x -> Array.copy cells0.(x)) in
  { wx; wy; p_x; p_y;
    cells=[| cells0; cells1 |];
    count=0; dir=Left; victory=Struggling; }

let get_cells state = state.cells.(state.count mod 2)

let fold_state ~f acc st =
  CCArray.foldi
    (fun acc x sub ->
      CCArray.foldi (fun acc y c -> f acc (x,y) c) acc sub)
    acc (get_cells st)

let fill cells x =
  Array.iter (fun a -> Array.fill a 0 (Array.length a) x) cells

let move state offset =
  let x = state.p_x in
  let y = state.p_y in
  let x' = x + offset in
  let cells = get_cells state in
  if cells.(x').(y) = Alien then state.victory <- Lost
  else (
    cells.(x).(y) <- Empty;
    cells.(x').(y) <- Player;
    state.p_x <- x';
  )

let move_left state =
  if state.p_x > 0 then move state ~-1

let move_right state =
  if state.p_x + 1 < state.wx then move state 1

let shoot state =
  let cells = get_cells state in
  cells.(state.p_x).(state.p_y+1) <- Shot

let write_alien cells x y =
  if cells.(x).(y) = Empty then cells.(x).(y) <- Alien

let count_aliens state =
  fold_state 0 state ~f:(fun n _ c -> if c=Alien then n+1 else n)

(* do one tick. [i] is the number of the frame, reset every
   second. We update monsters every second and bullets every frame *)
let update_state state i =
  let cells' = state.cells.((state.count + 1) mod 2) in
  fill cells' Empty;
  let next_dir = ref state.dir in
  fold_state () state
    ~f:(fun () (x,y) c -> match c with
        | Shot ->
            if y+1 < state.wy
            then cells'.(x).(y + 1) <- Shot;
        | Alien ->
            (* move alien following dir *)
            if i mod 5 = 0 then begin match state.dir with
              | Down d ->
                  next_dir := d;
                  write_alien cells' x (y-2);
                  if y-2 <= state.p_y
                    then state.victory <- Lost; (* reached bottom *)
              | Left ->
                  write_alien cells' (x-1) y;
                  if x-1=0 then next_dir := Down Right;
              | Right ->
                  write_alien cells' (x+1) y;
                  if x+2 = state.wx then next_dir := Down Left;
            end
            (* else just stay the same place *)
            else write_alien cells' x y
        | Player -> cells'.(x).(y) <- Player
        | Empty -> ()
    );
  state.dir <- !next_dir;
  (* switch cells/cells' *)
  state.count <- state.count + 1;
  if count_aliens state = 0 then state.victory <- Won;
  () (* TODO *)

(* render current state *)
let img_of_state state =
  let center_box x = x |> I.hsnap state.wx |> I.vsnap state.wy in
  match state.victory with
  | Lost ->
      I.(string A.(st bold ++ fg red) "you lost" |> center_box)
  | Won ->
      I.(string A.(st bold ++ fg green) "you won" |> center_box)
  | Struggling ->
    let i = fold_state I.empty state
      ~f:(fun img (x,y) c ->
          let set_pos = I.pad ~l:x ~t:(state.wy-y) in
          match c with
          | Empty -> img
          | Shot ->
              I.(img </> (string A.(fg red) "^" |> set_pos))
          | Player ->
              I.(img </> (string A.(fg green) "☭" |> set_pos))
          | Alien ->
              I.(img </> (string A.(fg lightmagenta) "×" |> set_pos))
      )
    in
    I.(i </> strf "frame %d" state.count)

let fps = 20

(* main game loop *)
let rec loop_render term state i =
  Term.image term (img_of_state state) >>= fun () ->
  Lwt_unix.sleep (1. /. float fps) >>= fun () ->
  update_state state i;
  loop_render term state ((i+1) mod fps)

let loop_input term state =
  Lwt_stream.iter
    (function
      | `Key (`Arrow `Left, _) -> move_left state
      | `Key (`Arrow `Right, _) -> move_right state
      | `Key (`ASCII ' ', _) -> shoot state
      | `Key (`ASCII 'q', _) -> raise Exit
      | `Resize _ -> failwith "resize is not supported"
      | _ -> ())
    (Term.events term)

(* initial screen *)
let print_help term wx wy =
  let msg = "left/right, space to shoot, q to quit" in
  let img =
    I.((string A.(st bold) msg |> hsnap wx
        <-> (string A.(st bold) "[enter]" |> hsnap wx))
      |> vsnap wy)
  in
  Term.image term img

let rec wait_for_enter term =
  match%lwt Lwt_stream.next (Term.events term) with
  | `Key (`Enter,_) -> Lwt.return_unit
  | _ -> wait_for_enter term

let main =
  let term = Term.create () in
  let wx, wy = Term.size term in
  print_help term wx wy >>= fun () ->
  wait_for_enter term >>= fun () ->
  (* start game *)
  let state = mk_state wx wy in
  try%lwt
    Lwt.pick [loop_render term state 1; loop_input term state]
  with Exit ->
    Term.release term >|= fun () ->
    print_endline "exit"

let () = Lwt_main.run main
