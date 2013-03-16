module Enigma.Emulator.Lib.DCPU16

// Readable/writeable register memory
module Registers =
  let mutable A = 0
  let mutable B = 0
  let mutable C = 0
  let mutable X = 0
  let mutable Y = 0
  let mutable Z = 0
  let mutable I = 0
  let mutable J = 0
  
  let get dst =
    match dst with
      | "A" -> A
      | "B" -> B
      | "C" -> C
      | "X" -> X
      | "Y" -> Y
      | "Z" -> Z
      | "I" -> I
      | "J" -> J
  
  let set dst src =
    match dst with
      | "A" -> A <- src
      | "B" -> B <- src
      | "C" -> C <- src
      | "X" -> X <- src
      | "Y" -> Y <- src
      | "Z" -> Z <- src
      | "I" -> I <- src
      | "J" -> J <- src