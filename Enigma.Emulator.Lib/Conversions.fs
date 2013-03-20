module Enigma.Emulator.Lib.Conversions
open System

type System.Convert with
  static member ToBits (i : int) l =
    let str = Convert.ToString (i, 2)
    let x = str.PadLeft (l, '0')
    x |> Seq.map (fun ch -> if ch = '1' then true else false) |> Seq.toList

// Note: when extracting, position 0 = MSB (usually the rightmost bit)
let extractBits n bStart bEnd =
  let mask = ~~~(~~~0 <<< (bEnd - bStart + 1))
  (n >>> bStart) &&& mask

// Split the instruction into its 3 components with the format:
// (destination : int, source : int, opcode : int)
let split n =
  (extractBits n 10 15,   // Destination
   extractBits n 5 9,     // Source
   extractBits n 0 4)     // Opcode