open Async
open Httpaf

type get
type post

type _ meth =
  | Get : get meth
  | Post : post meth

type ('meth, 'a) service = {
  meth : 'meth meth ;
  url : Uri.t ;
  req : Request.t ;
  encoding: 'a Json_encoding.encoding ;
  pp : Format.formatter -> 'a -> unit ;
  params : (string * string list) list ;
}

type auth = {
  key : string ;
  secret : string ;
}

type error =
  | Http of Client_connection.error
  | Kraken of string list

val time : (get, Ptime.t) service
val account_balance : (post, unit) service

val request : ?auth:auth -> (_, 'a) service -> ('a, error) result Deferred.t
