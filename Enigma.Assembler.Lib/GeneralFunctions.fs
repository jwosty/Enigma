module Enigma.Assembler.Lib.GeneralFunctions
open Microsoft.FSharp.Reflection
open Enigma.Assembler.Lib

// Adds item to list if item satisfies predicate
let addIf list predicate item = if predicate(item) then list @ [item] else list

let unionCaseName case = (fst <| FSharpValue.GetUnionFields(case, case.GetType())).Name

(*
let tokenToValue = function
  | RegA -> ()
  | RegB | RegC
  | RegX | RegY | RegZ
  | RegI | RegJ | SP
  | PC | EX
*)