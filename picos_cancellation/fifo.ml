open Printf
open Effect
open Effect.Deep
open Picos

module Queue = Saturn.Queue
module type S = sig
  val fork : (unit -> unit) -> Fiber.t
  val yield : unit -> unit
  val run : (unit -> unit) -> unit
  val cancel : Fiber.t -> unit
end

module Make () : S = struct

let suspend_count = Atomic.make 0
  
  type _ Effect.t += Fork  : (Fiber.t * (unit -> unit)) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t

  type 'a task = {k:('a, unit) Deep.continuation; fiber: Fiber.t}

  let run main =
    let run_q = Queue.create () in
    let enqueue (t: 'a task) =
      Queue.push run_q t;
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
              let tasks = 
              match Queue.pop run_q  with
              | Some task -> task in
              (if not (Fiber.is_canceled tasks.fiber) then
                begin
                  continue tasks.k ()
                end
              else
                begin
                  printf "Sched: A thread has been cancelled\n%!";
                  (* Do resource cleanup before discontinuing *)
                  try discontinue tasks.k Exit with
                  | Exit -> dequeue ()
                end
                )
            end
        end
    in
    let rec spawn ~new_fiber:fiber f =
      (* printf "Sched: Spawning \n%!"; *)
      match_with f ()
      { retc = (fun () -> dequeue ());
        exnc = ((fun ex ->
                   Printexc.raise_with_backtrace ex (Printexc.get_raw_backtrace ())));
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Trigger.Await t -> Some (fun (k: (a,_) Deep.continuation) ->
              let task1 = { k; fiber } in
              printf "Sched: Inside Await handler\n%!";
              let action trigger x _ =
                begin
                  printf "Resumer: Resumer executing\n%!";
                  enqueue (Obj.magic task1);
                  Atomic.decr suspend_count;
                end
              in 
              let Packed comp = Fiber.get_computation fiber in
              if Computation.try_attach comp t then
                (* Trigger.on_signal trigger x y resume *)
                begin
                if Trigger.on_signal t (Obj.magic k) (Obj.magic k) action then
                  begin
                    printf "Sched: On_signal successful, now dequeing\n%!";
                    Atomic.incr suspend_count;
                    dequeue ()       
                  end   
              end
              else
                continue k (Obj.magic ())    
              )
          | Yield -> Some (fun (k: (a,_) Deep.continuation) ->
              let t = { k; fiber } in 
              enqueue (Obj.magic t); 
              dequeue ())
          | Fork (new_fiber, f) -> Some (fun (k: (a,_) Deep.continuation) ->
              let t = { k; fiber } in
              enqueue (Obj.magic t); 
              spawn ~new_fiber f
              )
          | _ -> None }
    in
    let comp = Computation.create () in
    let new_fiber = Fiber.create ~forbid:false comp in
    (* let main = Computation.capture comp main () in *)
    spawn ~new_fiber main
    (* Computation.await comp *)
    

  let fork f = 
    printf "Sched: Calling perform\n%!";
    let comp = Computation.create () in
    let new_fiber = Fiber.create ~forbid:false comp in 
    perform (Fork (new_fiber, f));
    new_fiber
  
  let yield () = perform Yield

  let cancel fiber = 
    printf "Canceling the fiber\n%!";
    let (Packed computation) = Fiber.get_computation fiber in
    Computation.cancel computation (Exn_bt.get Exit)

end