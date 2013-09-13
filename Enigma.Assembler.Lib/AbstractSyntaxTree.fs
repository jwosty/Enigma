// See http://dcpu.com/dcpu-16/
module Enigma.Assembler.Lib.AbstractSyntaxTree

// Value union is currently incomplete
type Value =
  // Registers
  | RegA | RegB | RegC
  | RegX | RegY | RegZ
  | RegI | RegJ
  // Special registers
  | SP | PC | EX
  // A literal value, e.g. 0x2A
  | Literal of int16
  // Your traditional pointer, which appears in brackets in DCPU-16 assembly, e.g. [0x2A] vs 0x2A 
  | Pointer of Value