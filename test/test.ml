open Core
open Async

open Kraken_rest

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let wrap_request ?(speed=`Quick) n service =
  Alcotest_async.test_case n speed begin fun () ->
    request service >>= function
    | Ok v ->
      Logs_async.info (fun m -> m "%a" service.pp v) ;
    | Error _ -> failwith ""
  end

let rest = [
  wrap_request "time" time
]

let () =
  Alcotest.run "kraken" [
    "rest", rest ;
  ]
