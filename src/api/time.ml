
open CalendarLib
module C = Calendar.Precise

type t = C.t

let parse_date s =
  Printer.Precise_Calendar.from_fstring "%F %T" s

let extract_next_date current cached =
  let current, cached = parse_date current, parse_date cached in
  let period = C.sub current cached in
  C.add (C.now ()) period

let get_second_until t =
  let period = C.sub (C.now ()) t in
  C.Time.Period.to_seconds (C.Period.to_time period)
