module Enigma.Assembler.Lib.GeneralFunctions
open Microsoft.FSharp.Reflection
open Enigma.Assembler.Lib.Tokenizing
open Enigma.Assembler.Lib.Parsing

// Adds item to list if item satisfies predicate
let addIf list predicate item = if predicate(item) then list @ [item] else list

let unionCaseName case = (fst <| FSharpValue.GetUnionFields(case, case.GetType())).Name


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