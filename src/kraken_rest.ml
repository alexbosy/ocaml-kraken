open Core
open Async
open Httpaf

type auth = {
  key : string ;
  secret : string ;
}

type get
type post

type _ meth =
  | Get : get meth
  | Post : post meth

type ('meth, 'encoding) service = {
  meth : 'meth meth ;
  url : Uri.t ;
  req : Request.t ;
  encoding: 'encoding Json_encoding.encoding ;
  params : (string * string list) list ;
}

let src =
  Logs.Src.create "kraken.rest"

let base_url =
  Uri.make ~scheme:"https" ~host:"api.kraken.com" ()

let get ?(params=[]) encoding url =
  let target = Uri.path_and_query url in
  let req = Request.create `GET target in
  { meth = Get ; url ; req ; encoding ; params }

(* let post ?(params=[]) encoding url =
 *   let target = Uri.path_and_query url in
 *   let req = Request.create `POST target in
 *   { meth = Post ; url ; req ; encoding ; params } *)

let authstr ~secret service =
  let nonce = Time_ns.(to_int63_ns_since_epoch (now ())) in
  let encoded = Uri.encoded_of_query service.params in
  let digest =
    Digestif.SHA256.(digest_string ((Int63.to_string nonce) ^ encoded) |>
                     to_raw_string) in
  nonce,
  Digestif.SHA512.(hmac_string ~key:secret (Uri.path service.url ^ digest) |>
                   to_raw_string)

let write_iovec w iovec =
  List.fold_left iovec ~init:0 ~f:begin fun a { Faraday.buffer ; off ; len } ->
    Writer.write_bigstring w buffer ~pos:off ~len ;
    a+len
  end

let request (type a) ?auth (service : (a, 'b) service) =
  let error_iv = Ivar.create () in
  let response = Ivar.create () in
  let error_handler err =
    Ivar.fill error_iv (Error err)
  in
  let response_handler _response body =
    let buffer = Buffer.create 32 in
    let on_eof () =
      let resp_json = Ezjsonm.from_string (Buffer.contents buffer) in
      Ivar.fill response (Ok (Json_encoding.destruct service.encoding resp_json))
    in
    let on_read buf ~off ~len =
      Buffer.add_string buffer (Bigstringaf.substring buf ~off ~len)
    in
    Body.schedule_read body ~on_eof ~on_read in
  let headers = match service.params with
    | [] -> service.req.headers
    | _ ->
      Headers.add service.req.headers
        "content-type" "application/x-www-form-urlencoded" in
  let params, headers = match service.meth, auth with
    | Get, _ -> service.params, headers
    | Post, None -> invalid_arg "post service needs auth"
    | Post, Some { key ; secret } ->
      let nonce, a = authstr ~secret service in
      ("nonce", [Int63.to_string nonce]) :: service.params,
      Headers.add_list headers [
        "API-Key", key ;
        "API-Sign", Base64.encode_exn a ;
      ]
  in
  let req = { service.req with headers } in
  Conduit_async.V3.with_connection_uri service.url begin fun _ r w ->
    let body, conn =
      Client_connection.request req ~error_handler ~response_handler in
    let rec flush_req () =
      match Client_connection.next_write_operation conn with
      | `Write iovec ->
        let nb_read = write_iovec w iovec in
        Client_connection.report_write_result conn (`Ok nb_read) ;
        flush_req ()
      | `Yield ->
        Client_connection.yield_writer conn flush_req ;
      | `Close _ -> () in
    let rec read_response () =
      match Client_connection.next_read_operation conn with
      | `Close -> Deferred.unit
      | `Read -> begin
          Reader.read_one_chunk_at_a_time r
            ~handle_chunk:begin fun buf ~pos ~len ->
              Logs_async.debug ~src (fun m -> m "READ %d" len) >>= fun () ->
              let nb_read = Client_connection.read conn buf ~off:pos ~len in
              return (`Stop_consumed ((), nb_read))
            end >>= function
          | `Eof
          | `Eof_with_unconsumed_data _ ->
            raise Exit
          | `Stopped () ->
            read_response ()
        end in
    Logs_async.debug ~src
      (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
    begin
      match params with
      | [] -> ()
      | params ->
        let encoded_params = Uri.encoded_of_query params in
        Body.write_string body encoded_params
    end ;
    flush_req () ;
    don't_wait_for (read_response ()) ;
    Deferred.any [Ivar.read response ;
                  Ivar.read error_iv]
  end

(* type error = {
 *   severity : [`E | `W] ;
 *   cat : string ;
 *   msg : string ;
 *   extra : string option ;
 * } *)

let result_encoding encoding =
  let open Json_encoding in
  conv
    (function Error e -> (e, None) | Ok v -> [], Some v)
    (function (e, None) -> Error e | (_, Some r) -> Ok r)
    (obj2
       (req "error" (list string))
       (opt "result" encoding))

type 'a kraken_result = ('a, string list) result

let time =
  let time_encoding =
    let open Json_encoding in
    conv
      (fun t -> (), Int64.of_float (Ptime.to_float_s t))
      (fun ((), t) -> match Ptime.of_float_s (Int64.to_float t) with
         | None -> invalid_arg "time_encoding"
         | Some t -> t)
    (merge_objs unit (obj1 (req "unixtime" int53)))
  in
  get (result_encoding time_encoding) (Uri.with_path base_url "0/public/Time")
