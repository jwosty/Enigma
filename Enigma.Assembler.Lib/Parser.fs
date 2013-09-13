module Enigma.Assembler.Lib.Parser
open Enigma.Assembler.Lib
open GeneralFunctions
open Microsoft.FSharp.Reflection
open AbstractSyntaxTree
open Tokenizer

let nameOpcodeMap = FSharpType.GetUnionCases(typeof<Opcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray
let nameRegisterMap =
  let names = ["RegA"; "RegB"; "RegC"; "RegX"; "RegY"; "RegZ"; "RegI"; "RegJ"; "SP"; "PC"; "EX"]
  FSharpType.GetUnionCases(typeof<Value>)
  |> Array.filter (fun u -> List.exists ((=) u.Name) names)
  |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||]))
  |> Map.ofArray

// Converts a token to a case of a discriminated union using a map; returns none if not found
let tokenToCase tok (map: Map<_, string>) : Opcode option =
  try
    Some(unbox map.[unionCaseName tok])
  with
    :? System.Collections.Generic.KeyNotFoundException -> None