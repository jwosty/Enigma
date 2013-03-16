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