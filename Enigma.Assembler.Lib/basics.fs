module Enigma.Assembler.Basics
open System
open FParsec
open Ast

let enumNamesValues<'E when 'E : comparison> =
  let values = Enum.GetValues typeof<'E>
  let result =
    List.init
      (values.GetLength 0)
      (fun i ->
        let v = values.GetValue i
        (v |> string, unbox<'E> v))
    |> Map.ofList
  result

let basicOpcodes = enumNamesValues<BasicOpcode>
let specialOpcodes = enumNamesValues<SpecialOpcode>
let registers = enumNamesValues<Register>

// Parses whitespaces other newlines
let ws = manyChars (pchar ' ' <|> pchar '\t')
let letterGroup : Parser<string, unit> = many1Satisfy isLetter .>> ws

// Like Map.tryFind, but searches case-insensitively (only operates on string maps!)
let tryFindCI key (map : Map<string, _>) =
  let result = ref None
  map |> Map.iter (fun k v -> if String.Equals (key, k, System.StringComparison.CurrentCultureIgnoreCase) then result := Some v)
  !result

// Executes the given parser and applies the convertsion to the reult (on success)
let (>!) (parser : Parser<'TResult,_>) conversion =
  fun stream ->
    let reply = parser stream
    if reply.Status = Ok then
      conversion reply.Result
    else
      Reply(Error, reply.Error)

let letterGroupSearchParser (stuff : Map<_,_>) onMatch onMismatch onError : Parser<_,_> =
  fun stream ->
    let reply = letterGroup stream
    if reply.Status = Ok then
      match stuff |> tryFindCI reply.Result with
        | Some x -> onMatch x
        | None -> onMismatch reply
    else
      onError ()
let simpleLetterGroupSearchParser stuff mismatchMessage errorMessage =
  letterGroupSearchParser
    stuff
    (fun x -> Reply x)
    (fun reply -> Reply (Error, messageError <| mismatchMessage + " `" + reply.Result + "'"))
    (fun () -> Reply(Error, errorMessage))