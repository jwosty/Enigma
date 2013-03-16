module Enigma.Emulator.Lib.Instructions
open System
open Enigma.Emulator.Lib.Conversions

let all n be = List.init n (fun _ -> be)
  
// TODO: Add more registers (X, Y, Z, I, J)
type Register = 
  | A | B | C
  member this.Bits =
    match this with
      | A -> [false;false;false;false;false]
      | B -> [false;false;false;false;true]
      | C -> [false;false;false;true ;false]

type SourceOperand = 
  | Lit of int | Reg of Register
  member this.Bits =
    match this with
      | Reg r -> false :: r.Bits
      | Lit n -> true :: Convert.ToBits (n + 1) 5

type Instruction =
  | SET of Register*SourceOperand
  | ADD of Register*SourceOperand
  | SUB of Register*SourceOperand
  | MUL of Register*SourceOperand
  | DIV of Register*SourceOperand
  
  member this.Bits =
    let genBits (dst : Register) (src : SourceOperand) code = src.Bits @ dst.Bits @ (Convert.ToBits code 5)
    
    match this with
      | SET (dst, src) -> genBits dst src 0x01
      | ADD (dst, src) -> genBits dst src 0x02
      | SUB (dst, src) -> genBits dst src 0x03
      | MUL (dst, src) -> genBits dst src 0x04
      | DIV (dst, src) -> genBits dst src 0x06
  
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