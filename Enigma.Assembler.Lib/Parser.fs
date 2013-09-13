module Enigma.Assembler.Lib.Parser
open Enigma.Assembler.Lib
open GeneralFunctions
open Microsoft.FSharp.Reflection
open AbstractSyntaxTree
open Tokenizer

let opcodeNameCaseMap = FSharpType.GetUnionCases(typeof<Opcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray
let registerNameCaseMap =
  let names = ["RegA"; "RegB"; "RegC"; "RegX"; "RegY"; "RegZ"; "RegI"; "RegJ"; "SP"; "PC"; "EX"]
  FSharpType.GetUnionCases(typeof<Value>) |> Array.filter (fun u -> List.exists ((=) u.Name) names)

let tokenToOpcode tok : Opcode option =
  try
    Some(unbox opcodeNameCaseMap.[unionCaseName tok])
  with
    :? System.Collections.Generic.KeyNotFoundException -> None