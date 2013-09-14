// See http://dcpu.com/dcpu-16/
module Enigma.Assembler.Lib.Parsing.AbstractSyntaxTree

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

type BasicOpcode =
  | SET
  // Math
  | ADD | SUB
  | MUL | MLI | DIV | DVI
  | MOD | MDI
  // Bitwise logic
  | AND | BOR | XOR
  // Shifting
  | SHR | ASR | SHL
  // Branching
  | IFB | IFC | IFE | IFN
  | IFG | IFA | IFL | IFU
  // Other stuff
  | ADX | SBX
  | STI | STD
  
type SpecialOpcode =
  | JSR
  // Interrupt stuff
  | INT | IAG | IAS | IAQ
  // Hardware devices
  | HWN | HWQ | HWI

type Instruction =
  | BasicInstruction of BasicOpcode * Value * Value
  | SpecialInstruction of SpecialOpcode * Value