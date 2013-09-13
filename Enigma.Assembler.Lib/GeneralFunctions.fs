module Enigma.Assembler.Lib.GeneralFunctions
open Microsoft.FSharp.Reflection

// Adds item to list if item satisfies predicate
let addIf list predicate item = if predicate(item) then list @ [item] else list

let unionCaseName<'T> case = (fst <| FSharpValue.GetUnionFields(case, typeof<'T>)).Name