open Core
open Async

open Kraken_rest

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let wrap ?(speed=`Quick) n f =
  Alcotest_async.test_case n speed begin fun () ->
    f () >>= function
    | Ok _ -> Deferred.unit
    | Error _ -> failwith ""
  end

let rest = [
  wrap "time" (fun () -> request time)
]

let () =
  Alcotest.run "kraken" [
    "rest", rest ;
  ]
