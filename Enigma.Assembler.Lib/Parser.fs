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

let tokenToOpcode tok : Opcode option =
  try
    Some(unbox nameOpcodeMap.[unionCaseName tok])
  with
    :? System.Collections.Generic.KeyNotFoundException -> None