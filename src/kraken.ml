open Sexplib.Std

let strfloat =
  Json_encoding.(conv string_of_float float_of_string string)

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)
end

module OrdType = struct
  type t = [
    | `order_type_unset
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ]

  let encoding =
    let open Json_encoding in
    string_enum [
      "market", `order_type_market ;
      "limit", `order_type_limit ;
      "stop-loss", `order_type_stop ;
      "stop-loss-limit", `order_type_stop_limit ;
      "take-profit", `order_type_market_if_touched ;
    ]
end

module OrdStatus = struct
  type t = [
    | `order_status_pending_open
    | `order_status_open
    | `order_status_filled
    | `order_status_canceled
  ]

  let encoding : t Json_encoding.encoding =
    let open Json_encoding in
    string_enum [
      "pending", `order_status_pending_open ;
      "open", `order_status_open ;
      "closed", `order_status_filled ;
      "canceled", `order_status_canceled ;
      "expired", `order_status_canceled ;
    ]
end

module Balance = struct
  type t = {
    equivalent_balance : float ;
    trade_balance : float ;
    total_margin : float ;
    pnl : float ;
    positions_cost : float ;
    positions_value : float ;
    equity : float ;
    free_margin : float ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding =
    let open Json_encoding in
    conv
      (fun { equivalent_balance ;
             trade_balance ; total_margin ;
             pnl ; positions_cost ; positions_value ;
             equity ; free_margin } ->
        (equivalent_balance, trade_balance,
         total_margin, pnl, positions_cost, positions_value,
         equity, free_margin))
      (fun (equivalent_balance, trade_balance,
            total_margin, pnl, positions_cost, positions_value,
            equity, free_margin) ->
        { equivalent_balance ;
          trade_balance ; total_margin ;
          pnl ; positions_cost ; positions_value ;
          equity ; free_margin })
      (obj8
         (req "eb" strfloat)
         (req "tb" strfloat)
         (req "m" strfloat)
         (req "n" strfloat)
         (req "c" strfloat)
         (req "v" strfloat)
         (req "e" strfloat)
         (req "mf" strfloat))
end

module Order = struct
  type descr = {
    pair: string;
    (* type_: string [@key "type"]; *)
    ordertype: string;
    price: string;
    price2: string;
    leverage: string;
    order: string;
    (* close: (string [@default ""]); *)
  }

  type t = {
    refid: string option;
    userref: string option;
    status: string;
    opentm: float;
    starttm: float;
    expiretm: float;
    descr: descr;
    vol: string;
    vol_exec: string;
    cost: string;
    fee: string;
    price: string;
    (* stopprice: (string [@default ""]); *)
    (* limitprice: (string [@default ""]); *)
    (* misc: string; *)
    (* oflags: string; *)
    (* trades: (string list [@default []]); *)
  }
end

module Filled_order = struct
  type t = {
    ordertxid: string;
    pair: string;
    time: float;
    type_: string;
    ordertype: string;
    price: string;
    cost: string;
    fee: string;
    vol: string;
    margin: string;
    misc: string;
    (* closing: *)
    (* posstatus: *)
    (* cprice: *)
    (* ccost: *)
    (* cfee: *)
    (* cvol: *)
    (* cmargin: *)
    (* net: *)
    (* trades: *)
  }
end
