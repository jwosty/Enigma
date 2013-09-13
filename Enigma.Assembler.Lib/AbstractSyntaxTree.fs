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

type Opcode =
  // -----------------
  // - Basic Opcodes -
  // -----------------
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
  
  // -------------------
  // - Special Opcodes -
  // -------------------
  | JSR
  // Interrupt stuff
  | INT | IAG | IAS | IAQ
  // Hardware devices
  | HWN | HWQ | HWI

type Instruction =
  | BasicInstruction of Opcode * Value * Value
  | SpecialInstruction of Opcode * Value * Value