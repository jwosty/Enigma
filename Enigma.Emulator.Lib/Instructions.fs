module Enigma.Emulator.Lib.Instructions
open System
open Microsoft.FSharp.Reflection
open Enigma.Emulator.Lib.Conversions
open Enigma.Emulator.Lib.DCPU16

let getUnionCase (x : 'a) =
      match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

let all n be = List.init n (fun _ -> be)

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

// Represents a DCPU-16 instruction, like:
// SET A, 0
// which can be created directly with:
// SET(A, Lit 0)
type Instruction =
  | SET of Register*SourceOperand
  | ADD of Register*SourceOperand
  | SUB of Register*SourceOperand
  | MUL of Register*SourceOperand
  | DIV of Register*SourceOperand
  
  // Get the binary representation
  member this.Bits =
    let genBits (dst : Register) (src : SourceOperand) code = src.Bits @ dst.Bits @ (Convert.ToBits code 5)
    match this with
      | SET(dst, src) -> genBits dst src 0x01
      | ADD(dst, src) -> genBits dst src 0x02
      | SUB(dst, src) -> genBits dst src 0x03
      | MUL(dst, src) -> genBits dst src 0x04
      | DIV(dst, src) -> genBits dst src 0x06
  
  // Excecute the instruction
  member this.Eval () =
    match this with
      | SET(dst, src) -> Registers.set (getUnionCase dst) (src.Eval ())
  
  member this.Dump =
    let bits = this.Bits
    let str = String.init (bits |> List.length) (fun i -> if bits.[i] then "1" else "0")
    Convert.ToInt32 (str, 2)
  
  // Get the raw machine code instruction, e.g:
  // SET A, 0   --->   0x8401
  // and
  // SET A, B   --->   0x0401
  member this.HexDump =
    (this.Dump.ToString ("X")).PadLeft (4, '0')

// For FSI; doesn't get compiled (#if FALSE gets rid of annoying warnings)
#if FALSE
// Use to test Instruction.Bits
SET(B, Lit 0).Bits = [true; false; false; false; false; true;    false; false; false; false; true;     false; false; false; false; true]
SET(A, Lit 1).Bits = [true; false; false; false; true; false;    false; false; false; false; false;    false; false; false; false; true]
SET(A, Reg B).Bits = [false; false; false; false; false; true;   false; false; false; false; false;    false; false; false; false; true]
// Use to test Instruction.DumpHex ()
SET(B, Lit 0).HexDump = "8421"
SET(A, Lit 1).HexDump = "8801"
SET(A, Reg B).HexDump = "0401"
#endif