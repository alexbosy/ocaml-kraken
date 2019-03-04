open Async
open Httpaf

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

type auth = {
  key : string ;
  secret : string ;
}

type 'a kraken_result = ('a, string list) result

val time : (get, Ptime.t kraken_result) service

val request : ?auth:auth -> (_, 'a) service ->
  ('a, Client_connection.error) result Deferred.t
