let cycle dbh value maker compare =
  let view = << {x = $maker value$} >> in
  let res = Query.view_one dbh ~log:stdout view in
  let value' = res#!x in
  assert (compare value value' = 0)

open CalendarLib

let () =
  let dbh = PGOCaml.connect () in
  cycle dbh true Sql.Value.bool compare;
  cycle dbh 12 Sql.Value.int16 compare;
  cycle dbh 12l Sql.Value.int32 Int32.compare;
  cycle dbh 12L Sql.Value.int64 Int64.compare;
  cycle dbh 0.2 Sql.Value.float compare;
  cycle dbh "foo \\ bar" Sql.Value.string String.compare;
  let time = Time.now () in
  cycle dbh time Sql.Value.time Time.compare;
  let date = Date.from_unixfloat (Unix.time ()) in
  cycle dbh date Sql.Value.date Date.compare;
  let timestamp = Calendar.now () in
  cycle dbh timestamp Sql.Value.timestamp Calendar.compare;
  let timestamptz = timestamp, Time_Zone.current () in
  let tz_compare (t, tz) (t', tz') =
    let t2 = Calendar.convert t' tz' tz in
    Calendar.compare t t2 in
  cycle dbh timestamptz Sql.Value.timestamptz tz_compare
