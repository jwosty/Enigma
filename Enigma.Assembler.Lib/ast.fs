module Ast

type BasicOpcode =
  | SET = 0x01
  | ADD = 0x02
  | SUB = 0x03
  | MUL = 0x04
  | DIV = 0x06

type SpecialOpcode =
  | JSR = 0x01

type Opcode =
  | Basic of BasicOpcode
  | Special of SpecialOpcode

type Register =
  | A = 0x0
  | B = 0x1
  | C = 0x2
  | X = 0x3
  | Y = 0x4
  | Z = 0x5
  | I = 0x6
  | J = 0x7

type SourceOperand =
  | Lit of uint16
  | Reg of Register

type Instruction =
  | BasicInstruction of BasicOpcode * Register * SourceOperand
  | SpecialInstruction of SpecialOpcode * Register