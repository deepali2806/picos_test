open Effect
open Effect.Deep
open Printf
open Picos
module F = Fifo.Make ()

let m = MVar.create_empty ()

let main () =
    F.run (fun () ->
      printf "Main Scheduler: Before fiber 1\n%!";
      let fiber1 = F.fork (fun _ ->
        Printf.printf "Sched 1 Fiber 1: Thread in Lifo scheduler2 taking from MVar\n%!";
        (* F.yield(); *)
        let v = MVar.take m in
        (* let v = 30 in *)
        Printf.printf "Sched 1 Fiber 1:Thread in Lifo scheduler 2took %d from MVar\n%!" v) in 
        (* (); *)
      
        (* printf "Main Scheduler: Before fiber 2\n%!"; *)

      let fiber2 = F.fork (fun _ ->
          Printf.printf "Sched 1 : Fiber 2\n%!";
          Unix.sleep 2;
        let v = 42 in
        Printf.printf "Sched 2: Thread in Fifo scheduler2 putting %d into MVar\n%!" v;
        MVar.put v m;
          ) in (); 
          (* Computation.await (Fiber.get_computation fiber2); *)
        ) ;

  printf "\nBoth the fibers are done completed\n%!"

let _ = main ()