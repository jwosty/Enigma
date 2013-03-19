namespace Enigma.Emulator.Lib.Instructions
open System
open Microsoft.FSharp.Reflection
open Enigma.Emulator.Lib.Conversions
open Enigma.Emulator.Lib.DCPU16

type Register =
  | A = 0x0
  | B = 0x1
  | C = 0x2
  | X = 0x3
  | Y = 0x4
  | Z = 0x5
  | I = 0x6
  | J = 0x7

module Ops =
  let all n be = List.init n (fun _ -> be)
  
  let bits (reg : Register) = Convert.ToBits (reg |> int) 5
  
  // Get the value of this register
  let getReg (reg : Register) = Registers.get (reg |> string)
  
  // Set the value of this register
  let setReg (reg : Register) = Registers.set (reg |> string)

open Ops

// Can be either a literal value or a register
type SourceOperand = 
  | Lit of int | Reg of Register
  member this.Bits =
    match this with
      | Reg r -> false :: (r |> bits)
      | Lit n -> true :: Convert.ToBits (n + 1) 5
  
  // Get the value
  member this.Eval () =
    match this with
      // For regisers, read the register contents
      | Reg r -> Registers.get (r |> string)
      // For literal values, return the value
      | Lit n -> n

type OrdinaryOpcode =
  | SET = 0x01
  | ADD = 0x02
  | SUB = 0x03
  | MUL = 0x04
  | DIV = 0x06

type SpecialOpcode =
  | JSR = 0x01