module Client = Resto_cohttp.Client.Make(Resto_json.Encoding)
open Client

let balance =
  Service.post_service
    ~description:"Get trade balance"
    ~query:Resto.Query.empty
    ~input:
