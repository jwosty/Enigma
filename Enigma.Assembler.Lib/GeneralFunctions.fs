module Enigma.Assembler.Lib.GeneralFunctions

// Adds item to list if item satisfies predicate
let addIf list predicate item = if predicate(item) then list @ [item] else list