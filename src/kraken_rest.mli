open Async
open Httpaf
open Kraken

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

type balances = (string * float) list [@@deriving sexp]
val account_balance : (post, balances) service
val trade_balance : (post, Balance.t) service
val closed_orders : (post, Balance.t) service

val request : ?auth:auth -> (_, 'a) service -> ('a, error) result Deferred.t
