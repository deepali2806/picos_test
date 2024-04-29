open Effect
open Effect.Deep
open Printf

module F = Fifo.Make ()

let m = MVar.create_empty ()

let main () =
  let domain1 = Domain.spawn (fun () ->
    F.run (fun () ->

      F.fork (fun _ ->
        Printf.printf "Sched 1 Fiber 1: Thread in Lifo scheduler2 taking from MVar\n";
        let v = MVar.take m in
        Printf.printf "Sched 1 Fiber 1:Thread in Lifo scheduler 2took %d from MVar\n" v);

      F.fork (fun _ ->
          Printf.printf "Sched 1 : Fiber 2\n";
          )
        );
  ) in
  let comp () =
    F.run (fun () ->
      F.fork (fun _ ->
        Unix.sleep 2;
        let v = 42 in
        Printf.printf "Sched 2: Thread in Fifo scheduler2 putting %d into MVar\n" v;
        MVar.put v m;))
  in
  comp ();
  let _ = Domain.join domain1 in
    printf "\nBoth the domains are done completed\n%!"

  (* match_with comp ()
  { retc = (fun () -> ());
    exnc = (function
      | Exit -> ()
      | e -> Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
    effc = fun (type a) (e : a Effect.t) ->
      match e with
      | e -> None } *)

let _ = main ()