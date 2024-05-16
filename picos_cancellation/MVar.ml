open Effect
open Printf
open Picos
open Fun_queue

type 'a mv_state =
  | Full  of 'a * ('a * Trigger.t) Fun_queue.t
  | Empty of ('a Option.t ref* Trigger.t) Fun_queue.t

type 'a t = 'a mv_state Atomic.t

let create_empty () = Atomic.make (Empty (Fun_queue.empty))

let create v = Atomic.make (Full (v, Fun_queue.empty))

(* module Trigger_hashtable = Hashtbl.Make(
                            struct 
                            type t = 'a
                          end) *)
let hashtable = Hashtbl.create 16     

let triggerlist = ref []   

let rec find tlist trigger = 
    match tlist with
    | [] -> raise Exit
    | (t,v)::tail -> 
      if trigger = t then v
      else find tail trigger

let add tlist v =
    v :: tlist 

let rec put v mv =
  let old_contents = Atomic.get (mv) in
  match old_contents with
  | Full (v', q) -> 
    begin
      let t = Trigger.create () in
      let newQueue = Fun_queue.push q (v,t) in
      let new_contents = Full (v', newQueue) in
      if Atomic.compare_and_set mv old_contents new_contents then
      match Trigger.await t with
      | None -> () 
    end        
  | Empty q ->
      if Fun_queue.length q = 0 then 
          begin
            let new_contents = Full (v, Fun_queue.empty) in
            if not (Atomic.compare_and_set mv old_contents new_contents) then
            put v mv
          end     
      else
          match Fun_queue.pop q with
            | None -> ()
            | Some (x, newQueue) -> 
              let (value, resume_trigger) = x in
              let new_contents = Empty newQueue in
              if Atomic.compare_and_set mv old_contents new_contents then 
                begin
                  (* Do it aatomicallu later *)
                  value := Some (Obj.magic v);
                  (* printf "Mvar put: Adding to list now \n";
                  triggerlist := add !triggerlist (resume_trigger, (Obj.magic v));
                  printf "Mvar put: Signaling now \n"; *)
                  Trigger.signal resume_trigger
                end
              else
                put v mv

let rec take mv =
  let old_contents = Atomic.get mv in 
  match old_contents with
  | Empty q -> let t = Trigger.create () in 
              printf "Mvar take: Suspending\n";
              let new_entry = (ref None, t) in
              let newQueue = Fun_queue.push q new_entry in
              let new_contents = Empty newQueue in
              if Atomic.compare_and_set mv old_contents new_contents then
                begin
                  printf "Mvar take: Before await\n";
                  match Trigger.await t with
                        | None -> (
                                  let v = (fst new_entry) in
                                  (* let v = find !triggerlist t in 
                                  printf "Mvar take: Finally done\n";  *)
                                  match !v with
                                  | Some v' -> (Obj.magic v')
                                  )
                        | _ -> printf "Mvar take: Cancelled\n"; raise Exit
                end
              else
                take mv
                             
  | Full (v, q) ->
                if Fun_queue.length q = 0 then
                  begin
                    let new_contents = Empty Fun_queue.empty in
                    if Atomic.compare_and_set mv old_contents new_contents then
                      v
                    else 
                      take mv
                  end 
                else
                    match Fun_queue.pop q with
                    | None -> raise Exit
                    | Some ((v', resume_trigger), newQueue) -> 
                        let new_contents = Full (v', newQueue) in
                        if Atomic.compare_and_set mv old_contents new_contents then 
                          begin
                              Trigger.signal resume_trigger;
                              v
                          end
                        else
                          take mv   
(* open Effect
open Printf
open Picos
open Fun_queue

type 'a mv_state =
  | Full  of 'a * ('a * Trigger.t) Fun_queue.t
  | Empty of (Trigger.t) Fun_queue.t

type 'a t = 'a mv_state Atomic.t

let create_empty () = Atomic.make (Empty (Fun_queue.empty))

let create v = Atomic.make (Full (v, Fun_queue.empty))

(* module Trigger_hashtable = Hashtbl.Make(
                            struct 
                            type t = 'a
                          end) *)
let hashtable = Hashtbl.create 16     

let triggerlist = ref []   

let rec find tlist trigger = 
    match tlist with
    | [] -> raise Exit
    | (t,v)::tail -> 
      if trigger = t then v
      else find tail trigger

let add tlist v =
    v :: tlist 

let rec put v mv =
  let old_contents = Atomic.get (mv) in
  match old_contents with
  | Full (v', q) -> 
    begin
      let t = Trigger.create () in
      let newQueue = Fun_queue.push q (v,t) in
      let new_contents = Full (v', newQueue) in
      if Atomic.compare_and_set mv old_contents new_contents then
      match Trigger.await t with
      | None -> () 
    end        
  | Empty q ->
      if Fun_queue.length q = 0 then 
          begin
            let new_contents = Full (v, Fun_queue.empty) in
            if not (Atomic.compare_and_set mv old_contents new_contents) then
            put v mv
          end     
      else
          match Fun_queue.pop q with
            | None -> ()
            | Some (x, newQueue) -> 
              let resume_trigger = x in
              let new_contents = Empty newQueue in
              if Atomic.compare_and_set mv old_contents new_contents then 
                begin
                  printf "Mvar put: Adding to list now \n";
                  triggerlist := add !triggerlist (resume_trigger, (Obj.magic v));
                  printf "Mvar put: Signaling now \n";
                  Trigger.signal resume_trigger
                end
              else
                put v mv

let rec take mv =
  let old_contents = Atomic.get mv in 
  match old_contents with
  | Empty q -> let t = Trigger.create () in 
              printf "Mvar take: Suspending\n";
              let newQueue = Fun_queue.push q t in
              let new_contents = Empty newQueue in
              if Atomic.compare_and_set mv old_contents new_contents then
                begin
                  printf "Mvar take: Before await\n";
                  match Trigger.await t with
                        | None -> (
                                  let v = find !triggerlist t in 
                                  printf "Mvar take: Finally done\n"; 
                                  (Obj.magic v)
                                  )
                        | _ -> printf "Mvar take: Cancelled\n"; raise Exit
                end
              else
                take mv
                             
  | Full (v, q) ->
                if Fun_queue.length q = 0 then
                  begin
                    let new_contents = Empty Fun_queue.empty in
                    if Atomic.compare_and_set mv old_contents new_contents then
                      v
                    else 
                      take mv
                  end 
                else
                    match Fun_queue.pop q with
                    | None -> raise Exit
                    | Some ((v', resume_trigger), newQueue) -> 
                        let new_contents = Full (v', newQueue) in
                        if Atomic.compare_and_set mv old_contents new_contents then 
                          begin
                              Trigger.signal resume_trigger;
                              v
                          end
                        else
                          take mv    *)