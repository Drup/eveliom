include CalendarLib.Calendar

type 'a apiperiod =
  ([< CalendarLib.Period.date_field > `Day `Week ] as 'a)
    Period.period


let parse_date s =
  CalendarLib.Printer.Calendar.from_fstring "%F %T" s

let extract_next_date current cached =
  let current, cached = parse_date current, parse_date cached in
  let period = sub current cached in
  add (now ()) period

let to_seconds d =
    Time.Period.to_seconds (Period.to_time d)

let time_until t = sub (now ()) t
