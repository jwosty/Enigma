namespace Enigma.Emulator.Lib.Instructions
open System
open Microsoft.FSharp.Reflection
open Enigma.Emulator.Lib.Conversions
open Enigma.Emulator.Lib.DCPU16

module Ops =
  let getUnionCase (x : 'a) =
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
          | case, _ -> case.Name

  let all n be = List.init n (fun _ -> be)

open Ops

type Register = 
  | A | B | C | X | Y | Z | I | J
  member this.Bits =
    match this with
      | A -> Convert.ToBits 0 5
      | B -> Convert.ToBits 1 5
      | C -> Convert.ToBits 2 5
      | X -> Convert.ToBits 3 5
      | Y -> Convert.ToBits 4 5
      | Z -> Convert.ToBits 5 5
      | I -> Convert.ToBits 6 5
      | J -> Convert.ToBits 7 5
  
  // Get the value of this register
  member this.Get () = Registers.get (getUnionCase this)
  
  // Set the value of this register
  member this.Set = Registers.set (getUnionCase this)

// Can be either a literal value or a register
type SourceOperand = 
  | Lit of int | Reg of Register
  member this.Bits =
    match this with
      | Reg r -> false :: r.Bits
      | Lit n -> true :: Convert.ToBits (n + 1) 5
  
  // Get the value
  member this.Eval () =
    match this with
      // For regisers, read the register contents
      | Reg r -> Registers.get (getUnionCase r)
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