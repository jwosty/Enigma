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
let registers = enumNamesValues<Register>
let isBasicOpcode x =
  match basicOpcodes.TryFind x with
    | Some _ -> true
    | None -> false

let letterGroupSearchParser (stuff : Map<_,_>) onMatch onMismatch onError : Parser<_,_> =
  fun stream ->
    let reply = letterGroup stream
    if reply.Status = Ok then
      match stuff.TryFind reply.Result with
        | Some x -> onMatch x
        | None -> onMismatch reply
    else
      onError ()

let letterGroupSearchParserMessage stuff onMatch mismatchMessage errorMessage =
  letterGroupSearchParser
    stuff
    onMatch
    (fun reply -> Reply (Error, messageError <| mismatchMessage + " `" + reply.Result + "'"))
    (fun () -> Reply(Error, errorMessage))

let destinationOperand = letterGroupSearchParserMessage registers (fun x -> Reply x) "No such register" (expected "Destination operand")

let basicOpcode = letterGroupSearchParserMessage basicOpcodes (fun x -> Reply x) "No such opcode" (expected "Two-argument opcode")

(*
// Note to self: refactor this and basicOpcode -- lots of duplicate code!
// For now, destination operand can only be a register
let destinationOperand : Parser<Register,_> =
  fun stream ->
    let reply = letterGroup stream
    if reply.Status = Ok then
      match registers.TryFind reply.Result with
      | Some x -> Reply x
      | None -> Reply (Error, messageError <| "No such register `" + reply.Result + "'")
    else
      Reply(Error, expected <| "Destination operand")

// Try: CharStream.ParseString ("ADD SUB", 0, "ADD SUB".Length, basicOpcode, (), "")
let basicOpcode : Parser<Opcode,_> =
  fun stream ->
    let reply = letterGroup stream
    if reply.Status = Ok then
      match basicOpcodes.TryFind reply.Result with
      | Some x -> Reply (Basic x)
      | None -> Reply (Error, messageError <| "No such opcode `" + reply.Result + "'")
    else
      Reply(Error, expected <| "Two-argument opcode")
*)