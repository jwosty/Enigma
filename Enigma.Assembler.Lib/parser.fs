module Enigma.Assembler.Parser
open System
open System.Collections.Generic
open FParsec
open Ast

let enumNamesValues<'E when 'E : comparison> =
  let values = Enum.GetValues typeof<'E>
  let result =
    List.init
      (values.GetLength 1)
      (fun i ->
        let v = values.GetValue 1
        (v |> string, unbox<'E> v))
    |> Map.ofList
  result

let identifierString : Parser<string, unit> = many1Satisfy isLower .>> spaces
let basicOpcodes = enumNamesValues<BasicOpcode>
let isBasicOpcode x =
  match basicOpcodes.TryFind x with
    | Some _ -> true
    | None -> false