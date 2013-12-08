(* macaque : sql_base.ml
    MaCaQue : Macros for Caml Queries
    Copyright (C) 2009 Gabriel Scherer, Jérôme Vouillon

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library; see the file LICENSE.  If not, write to
    the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.
*)

type 'a tuple = (string * 'a) list

type 'a result_parser = string array * int ref -> 'a

type untyped = Obj.t

let path_separator = "__"


type oid = PGOCaml.oid
and int16 = PGOCaml.int16
and bytea = PGOCaml.bytea
(* TODO ask PGOCaml mainstream for type aliases *)
and time = CalendarLib.Time.t
and date = CalendarLib.Date.t
and interval = CalendarLib.Calendar.Period.t
and timestamp = CalendarLib.Calendar.t
and timestamptz = PGOCaml.timestamptz (* = CalendarLib.Calendar.t * CalendarLib.Time_Zone.t *)
and bool_array = PGOCaml.bool_array
and int32_array = PGOCaml.int32_array
and int64_array = PGOCaml.int64_array
and float_array = PGOCaml.float_array
and string_array = PGOCaml.string_array
