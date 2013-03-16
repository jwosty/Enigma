module Enigma.Emulator.Lib.Instructions
open System

let toBits (i : int) l =
  let str = Convert.ToString (i, 2)
  let x = str.PadLeft (l, '0')
  x |> Seq.map (fun ch -> if ch = '1' then true else false) |> Seq.toList

// TODO: Add more registers (X, Y, Z, I, J)
type Register = 
  | A | B | C
  member this.bits =
    match this with
      | A -> [false;false;false;false;false]
      | B -> [false;false;false;false;true]
      | C -> [false;false;false;true ;false]

type SourceOperand = 
  | Lit of int | Reg of Register
  member this.bits =
    match this with
      | Reg r -> false :: r.bits
      | Lit n -> true :: toBits (n + 1) 5

let all n be = List.init n (fun _ -> be)

type Instruction =
  | SET of Register*SourceOperand | HLT
  member this.bits =
    match this with
      | SET (dst, src) -> src.bits @ dst.bits @ [false;false;false;false;true]
      | HLT            -> all 6 false @ all 5 false @ all 5 true

// For FSI; doesn't get compiled (#if FALSE gets rid of annoying warnings)
#if FALSE
SET(B, Lit 0).bits = [true; false; false; false; false; true;    false; false; false; false; true;     false; false; false; false; true]
SET(A, Lit 1).bits = [true; false; false; false; true; false;    false; false; false; false; false;    false; false; false; false; true]
SET(A, Reg B).bits = [false; false; false; false; false; true;   false; false; false; false; false;    false; false; false; false; true]
#endif