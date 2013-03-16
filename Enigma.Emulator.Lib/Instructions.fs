module Enigma.Emulator.Lib.Instructions
open System
open Microsoft.FSharp.Reflection
open Enigma.Emulator.Lib.Conversions
open Enigma.Emulator.Lib.DCPU16

let getUnionCase (x : 'a) =
      match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

let all n be = List.init n (fun _ -> be)
  
// TODO: Add more registers (X, Y, Z, I, J)
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

type SourceOperand = 
  | Lit of int | Reg of Register
  member this.Bits =
    match this with
      | Reg r -> false :: r.Bits
      | Lit n -> true :: Convert.ToBits (n + 1) 5
  
  member this.Eval () =
    match this with
      | Reg r -> Registers.get (getUnionCase r)
      | Lit n -> n

type Instruction =
  | SET of Register*SourceOperand
  | ADD of Register*SourceOperand
  | SUB of Register*SourceOperand
  | MUL of Register*SourceOperand
  | DIV of Register*SourceOperand
  
  member this.Bits =
    let genBits (dst : Register) (src : SourceOperand) code = src.Bits @ dst.Bits @ (Convert.ToBits code 5)
    match this with
      | SET(dst, src) -> genBits dst src 0x01
      | ADD(dst, src) -> genBits dst src 0x02
      | SUB(dst, src) -> genBits dst src 0x03
      | MUL(dst, src) -> genBits dst src 0x04
      | DIV(dst, src) -> genBits dst src 0x06
  
  member this.Eval () =
    match this with
      | SET(dst, src) -> Registers.set (getUnionCase dst) (src.Eval ())
  
  // Get the raw machine code instruction, e.g:
  // SET A, 0   --->   0x8401
  // and
  // SET A, B   --->   0x0401
  member this.DumpHex () =
    let bits = this.Bits
    let str = String.init (bits |> List.length) (fun i -> if bits.[i] then "1" else "0")
    ((Convert.ToInt32 (str, 2)).ToString ("X")).PadLeft (4, '0')

// For FSI; doesn't get compiled (#if FALSE gets rid of annoying warnings)
#if FALSE
SET(B, Lit 0).bits = [true; false; false; false; false; true;    false; false; false; false; true;     false; false; false; false; true]
SET(A, Lit 1).bits = [true; false; false; false; true; false;    false; false; false; false; false;    false; false; false; false; true]
SET(A, Reg B).bits = [false; false; false; false; false; true;   false; false; false; false; false;    false; false; false; false; true]
#endif