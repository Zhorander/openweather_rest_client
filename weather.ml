open Core
open Lwt

module Config = struct
  type t = {
    city_id: string;
    api_key: string;
    units: string
  } [@@deriving sexp]

  let default_file = "config.sexp"

  let create_template () =
    let template = sexp_of_t
      {city_id="city_id";api_key="api_key";units="units"}
      |> Sexp.to_string
    in
    Out_channel.write_all ~data:template default_file
  
  let read fnam =
    let ic = match fnam with
      None -> default_file
      | Some nam -> nam
    in
    In_channel.read_all ic
    |> Sexp.of_string
    |> t_of_sexp
end

(* Generate a call to the open weaather api *)
let query_uri city apikey units =
  let base_uri = Uri.of_string "http://api.openweathermap.org/data/2.5/weather?" in
  List.fold_left
    ~init:base_uri
    ~f:Uri.add_query_param
    [("id",[city]);("appid",[apikey]);("units",[units])]

let find_json json key =
  match json with
    | `Assoc kv_list ->
        List.Assoc.find ~equal:String.equal kv_list key
    | _ -> None

let get_weather_desc json =
  let wobjs = Yojson.Safe.from_string json in
  match find_json wobjs "weather" with
    | Some (`List wlist) ->
      begin match List.nth wlist 0 with
        | None -> "Failed to get weather data"
        | Some obj ->
          begin match find_json obj "description" with 
          | Some (`String desc) -> desc
          | None | Some _ -> "Failed to get weather data"
          end
      end
    | None | Some _ -> "Failed to get weather data"

let get_weather_temp json =
  let wobjs = Yojson.Safe.from_string json in
  match find_json wobjs "main" with
    | Some obj ->
      begin match find_json obj "temp" with
      | Some (`Float temp) -> Float.to_string temp
      | None | Some _ -> "Failed to get temperature2"
      end
    | None -> "Failed to get temperature"

let get_weather_data config =
  let open Cohttp_lwt in
  let open Cohttp_lwt_unix in
  let Config.({city_id;api_key;units}) = config in
  let%lwt (_, body) = Client.get (query_uri city_id api_key units) in
  let%lwt json = Body.to_string body in
  let desc = get_weather_desc json in
  let temp = get_weather_temp json in
  return (desc, temp)

let print_weather (desc,temp) config =
  let suffix = match Config.(config.units) with
    | "imperial" -> "F"
    | "metric" -> "C"
    | _ -> ""
  in
  Lwt_io.printf "%s: %s%s" desc temp suffix

let main () =
  try%lwt
    let config = Config.read (Some Config.default_file) in
    let%lwt info = get_weather_data config in
    print_weather info config
  with Sys_error _ ->
    let%lwt () = Lwt_io.printf "Created template file\n" in
    return (Config.create_template ())

let () =
  Lwt_main.run (main ())