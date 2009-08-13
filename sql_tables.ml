(* macaque : sql_tables.ml
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

open Sql_internals
open Sql_types

type 'a table =
  { descr : types_descr;
    result_parser : 'a result_parser;
    name : table_name }

let table_view table =
  { Sql_internals.descr = table.descr;
    Sql_internals.result_parser = Sql_parsers.unsafe_parser table.result_parser;
    Sql_internals.concrete = Table table.name }

let table descr custom_result_parser name =
  { descr = descr;
    result_parser = custom_result_parser poly_parser;
    name = name }
