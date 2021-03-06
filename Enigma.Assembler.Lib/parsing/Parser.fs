module Enigma.Assembler.Lib.Parsing.Parser
open Microsoft.FSharp.Reflection
open Enigma.Assembler.Lib.GeneralFunctions
open Enigma.Assembler.Lib.Tokenizing
open Enigma.Assembler.Lib.Parsing
open Enigma.Assembler.Lib.Parsing.AbstractSyntaxTree

let nameBasicOpcodeMap = FSharpType.GetUnionCases(typeof<AbstractSyntaxTree.BasicOpcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray
let nameSpecialOpcodeMap = FSharpType.GetUnionCases(typeof<AbstractSyntaxTree.SpecialOpcode>) |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||])) |> Map.ofArray
let nameRegisterMap =
  let names = ["RegA"; "RegB"; "RegC"; "RegX"; "RegY"; "RegZ"; "RegI"; "RegJ"; "SP"; "PC"; "EX"]
  FSharpType.GetUnionCases(typeof<AbstractSyntaxTree.Value>)
  |> Array.filter (fun u -> List.exists ((=) u.Name) names)
  |> Array.map (fun caseInfo -> caseInfo.Name, FSharpValue.MakeUnion(caseInfo, [||]))
  |> Map.ofArray

let tokenToValue = function
  | Token.RegA -> Some AbstractSyntaxTree.RegA | Token.RegB -> Some AbstractSyntaxTree.RegB | Token.RegC -> Some AbstractSyntaxTree.RegC
  | Token.RegX -> Some AbstractSyntaxTree.RegX | Token.RegY -> Some AbstractSyntaxTree.RegX | Token.RegZ -> Some AbstractSyntaxTree.RegZ
  | Token.RegI -> Some AbstractSyntaxTree.RegI | Token.RegJ -> Some AbstractSyntaxTree.RegJ | Token.SP -> Some AbstractSyntaxTree.SP
  | Token.PC -> Some AbstractSyntaxTree.PC | Token.EX -> Some AbstractSyntaxTree.EX
  | _ -> None

let tokenToBasicOpcode = function
  | Token.SET -> Some AbstractSyntaxTree.SET
  | Token.ADD -> Some AbstractSyntaxTree.ADD | Token.SUB -> Some AbstractSyntaxTree.SUB
  | Token.MUL -> Some AbstractSyntaxTree.MUL | Token.MLI -> Some AbstractSyntaxTree.MLI | Token.DIV -> Some AbstractSyntaxTree.DIV | Token.DVI -> Some AbstractSyntaxTree.DVI
  | Token.AND -> Some AbstractSyntaxTree.AND | Token.BOR -> Some AbstractSyntaxTree.BOR | Token.XOR -> Some AbstractSyntaxTree.XOR
  | Token.SHR -> Some AbstractSyntaxTree.SHR | Token.ASR -> Some AbstractSyntaxTree.ASR | Token.SHL -> Some AbstractSyntaxTree.SHL
  | Token.IFB -> Some AbstractSyntaxTree.IFB | Token.IFC -> Some AbstractSyntaxTree.IFC | Token.IFE -> Some AbstractSyntaxTree.IFE | Token.IFN -> Some AbstractSyntaxTree.IFE
  | Token.IFG -> Some AbstractSyntaxTree.IFG | Token.IFA -> Some AbstractSyntaxTree.IFA | Token.IFL -> Some AbstractSyntaxTree.IFL | Token.IFU -> Some AbstractSyntaxTree.IFU
  | Token.ADX -> Some AbstractSyntaxTree.ADX | Token.SBX -> Some AbstractSyntaxTree.SBX | Token.STI -> Some AbstractSyntaxTree.STI | Token.STD -> Some AbstractSyntaxTree.STD
  | _ -> None

let tokenToSpecialOpcode = function
  | Token.JSR -> Some AbstractSyntaxTree.JSR
  | Token.INT -> Some AbstractSyntaxTree.INT | Token.IAG -> Some AbstractSyntaxTree.IAG | Token.IAS -> Some AbstractSyntaxTree.IAS | Token.IAQ -> Some AbstractSyntaxTree.IAQ
  | Token.HWN -> Some AbstractSyntaxTree.HWN | Token.HWQ -> Some AbstractSyntaxTree.HWQ | Token.HWI -> Some AbstractSyntaxTree.HWI
  | _ -> None

let tokenToComma = function
  | Token.Comma -> Some Token.Comma
  | _ -> None

let tokenToNewlines = function
  | Token.Newlines -> Some Token.Newlines
  | Token.EOF -> Some Token.EOF
  | _ -> None

let tokenToLBracket = function
  | Token.LeftBracket -> Some Token.LeftBracket
  | _ -> None

let tokenToRBracket = function
  | Token.RightBracket -> Some Token.LeftBracket
  | _ -> None

// Converts a token to a case of a discriminated union using a map; returns none if not found
let tokenToCase (map: Map<_, _>) tok : 'a option =
  try
    Some(map.[unionCaseName tok])
  with
    :? System.Collections.Generic.KeyNotFoundException -> None

let ret x = x

let tryParse tokenToASTConversion onSucceed onFailure tokens =
  let tok = List.head tokens
  match (tokenToASTConversion tok) with
    | Some x -> onSucceed(x, List.tail tokens)
    | None -> onFailure tok

let tryParseError tokenToASTConversion expectedName onSucceed tokens =
  tryParse tokenToASTConversion onSucceed (fun tok -> failwith <| sprintf "Syntax Error: Expecting %s, got `%A'" expectedName tok) tokens

let parseValue tokens = tryParseError tokenToValue "Value" ret tokens

// Parses a single operand
let parseOperand tokens =
  let b, r = tokenToLBracket (List.head tokens), List.tail tokens
  match b with
    | Some _ ->
      let value, r = parseValue r
      let _, r = tryParseError tokenToRBracket "Right Bracket" ret r
      Pointer(value), r
    | None ->
      parseValue tokens

// Parses two operands separated by commas
let parseOperands tokens =
  let src, r = parseOperand tokens
  let _, r = tryParseError tokenToComma "Comma" ret r
  let dst, r = parseOperand r
  src, dst, r

// Parses a single instruction (basic or special)
let parseInstruction tokens =
  tryParse
    <| tokenToBasicOpcode
    <| (fun (basicOpcode, r) ->
      let src, dst, r = parseOperands r
      BasicInstruction(basicOpcode, src, dst), r)
    <| (fun _ ->
      tryParseError
        <| tokenToSpecialOpcode
        <| "Basic Opcode or Special Opcode"
        <| (fun (specialOpcode, r) ->
          let src, r = parseOperand r
          SpecialInstruction(specialOpcode, src), r)
        <| tokens)
    <| tokens

let parse =
  // Parses multiple instructions separated by newlines until EOF is reached
  let rec parse prevInstructions tokens: Instruction list =
    match tokens with
      | [] -> prevInstructions
      | [EOF] -> prevInstructions
      | _ ->
        let instruction, r = parseInstruction tokens
        let _, r = tryParseError tokenToNewlines "Newline or EOF" ret r
        parse (prevInstructions @ [instruction]) r
  parse []