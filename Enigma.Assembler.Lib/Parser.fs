module Enigma.Assembler.Lib.Parser
open Enigma.Assembler.Lib
open GeneralFunctions
open Microsoft.FSharp.Reflection
open AbstractSyntaxTree
open Tokenizer

let opcodeNameCaseMap = FSharpType.GetUnionCases(typeof<Opcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray

let tokenToOpcode tok =
  try
    Some(opcodeNameCaseMap.[unionCaseName tok])
  with
    :? System.Collections.Generic.KeyNotFoundException -> None