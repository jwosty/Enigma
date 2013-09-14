module Enigma.Assembler.Lib.Parser
open Enigma.Assembler.Lib
open GeneralFunctions
open Microsoft.FSharp.Reflection
open AbstractSyntaxTree
open Tokenizer

let nameBasicOpcodeMap = FSharpType.GetUnionCases(typeof<BasicOpcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray
let nameSpecialOpcodeMap = FSharpType.GetUnionCases(typeof<SpecialOpcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray
let nameRegisterMap =
  let names = ["RegA"; "RegB"; "RegC"; "RegX"; "RegY"; "RegZ"; "RegI"; "RegJ"; "SP"; "PC"; "EX"]
  FSharpType.GetUnionCases(typeof<Value>)
  |> Array.filter (fun u -> List.exists ((=) u.Name) names)
  |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||]))
  |> Map.ofArray

// Converts a token to a case of a discriminated union using a map; returns none if not found
let tokenToCase (map: Map<_, _>) tok : 'a option =
  try
    Some(map.[unionCaseName tok])
  with
    :? System.Collections.Generic.KeyNotFoundException -> None

// Parses a single operand
let parseOperand tokens =
  ()

// Parses two operands separated by commas
let parseOperands tokens =
  ()

// Parses tokens until an syntax error or a newline token is reached
// TODO: Implement pointers
let parseStatement tokens =
  let tok, r = tokenToCase nameBasicOpcodeMap (List.head tokens), List.tail
  match tok with
    | Some tok ->
      BasicInstruction(unbox tok, Literal 42s, Literal 43s)
    | None ->
      let (tok: SpecialOpcode option), r = unbox tokenToCase nameSpecialOpcodeMap (List.head tokens), List.tail
      match tok with
        | Some tok ->
          SpecialInstruction(unbox tok, Literal 42s)
        | None -> failwith "Expected opcode"