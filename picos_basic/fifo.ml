open Printf
open Effect
open Effect.Deep
open Picos

module type S = sig
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
end

module Make () : S = struct

  type _ Effect.t += Fork  : (unit -> unit) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t

  let run main =
    let suspend_count = Atomic.make 0 in
    let run_q = Queue.create () in
    let enqueue t v =
      begin
        (* printf "Sched: Enqueued\n"; *)
        Queue.push (fun () -> continue t ()) run_q
      end
    in
    let rec dequeue () =
      if (Queue.is_empty run_q) then
        begin
          if (Atomic.get suspend_count > 0) then 
            dequeue()
          else
            ()
        end
      else 
        begin
          if (not (Queue.is_empty run_q)) then
            begin
              (* printf "Sched: Popping from q\n"; *)
              Queue.pop run_q (); 
              dequeue ()
            end
        end
    in
    let rec spawn f =
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Yield -> Some (fun (k: (a,_) continuation) ->
              enqueue k (); dequeue ())
          | Fork f -> Some (fun (k: (a,_) continuation) ->
              enqueue k (); spawn f)
          | Trigger.Await t -> Some (fun (k: (a,_) continuation) ->
              printf "Sched: Inside Await handler\n";
              let action trigger x _ =
                begin
                  printf "Resumer: Resumer executing\n";
                  Atomic.decr suspend_count;
                  enqueue x ();
                end
              in 
              if Trigger.on_signal t (Obj.magic k) (Obj.magic k) action then
                begin
                  Atomic.incr suspend_count;
                  dequeue ()       
                end   
              else
                continue k (Obj.magic ())    
              )
          | e -> None }
    in
    spawn main

  let fork f = perform (Fork f)
  let yield () = perform Yield
end