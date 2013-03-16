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
  | SET of Register*SourceOperand | HLT
  
  member this.Bits =
    match this with
      | SET (dst, src) -> src.Bits @ dst.Bits @ [false;false;false;false;true]
      | HLT            -> all 6 false @ all 5 false @ all 5 true
  
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