module Enigma.Assembler.Parser
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

let letterGroup : Parser<string, unit> = many1Satisfy isLetter .>> spaces
let basicOpcodes = enumNamesValues<BasicOpcode>
let specialOpcodes = enumNamesValues<SpecialOpcode>
let registers = enumNamesValues<Register>
// A comma that can be surrounded by whitespaces
let argSep : Parser<string, unit> = spaces >>. pstring "," .>> spaces
// Like Map.tryFind, but searches case-insensitively (only operates on string maps!)
let tryFindCI key (map : Map<string, _>) =
  let result = ref None
  map |> Map.iter (fun k v -> if String.Equals (key, k, System.StringComparison.CurrentCultureIgnoreCase) then result := Some v)
  !result

let transformParserOutput (parser : Parser<'Result,_>) conversion =
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

let register = simpleLetterGroupSearchParser registers "Invalid register" (expected "register")

// For now, just use registers
let destinationOperand = simpleLetterGroupSearchParser registers "No such register" (expected "Destination operand")
// For now, just use registers
let sourceOperand : Parser<_,_> = transformParserOutput register (fun x -> Reply(Reg x))

let basicOpcode = simpleLetterGroupSearchParser basicOpcodes "No such opcode" (expected "Two-argument opcode")
let specialOpcode = simpleLetterGroupSearchParser specialOpcodes "No such opcode" (expected "One-argument opcode")

let basicInstruction : Parser<_,_> =
  transformParserOutput
    (basicOpcode .>>. destinationOperand .>> argSep .>>. sourceOperand)
    (fun ((op, dst), src) -> Reply (BasicInstruction (op, dst, src)))
let specialInstruction : Parser<_,_> =
  transformParserOutput
    (specialOpcode .>>. destinationOperand)
    (fun (op, dst) -> Reply (SpecialInstruction (op, dst)))