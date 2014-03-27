
let get_size = function
  | `T32 -> "32" | `T64 -> "64" | `T128 -> "128"
  | `T256 -> "256" | `T512 -> "512"

let tq = "http://image.eveonline.com"
let test = "http://image.testeveonline.com"

type format = PNG | JPG
let format_to_string = function
  | PNG -> ".png"
  | JPG -> ".jpg"

let get api ty x size format =
  let s =
    Printf.sprintf "%s%s%i_%s%s"
      api ty x (get_size size) (format_to_string format)
  in Eliom_content.Html5.F.uri_of_string (fun () -> s)

let ally ?(api=tq) (size : [< `T32 | `T64 | `T128 ]) x =
  get api "/Alliance/" x size PNG

let corp ?(api=tq) (size : [< `T32 | `T64 | `T128 | `T256]) x =
  get api "/Corporation/" x size PNG

let char ?(api=tq) (size : [< `T32 | `T64 | `T128 | `T256 | `T512]) x =
  get api "/Character/" x size JPG

let typ ?(api=tq) (size : [< `T32 | `T64 ]) x =
  get api "/Type/" x size PNG

let render ?(api=tq) (size : [< `T32 | `T64 | `T128 | `T256 | `T512]) x =
  get api "/Render/" x size PNG
