module Enigma.Emulator.Lib.Conversions
open System

type System.Convert with
  static member ToBits (i : int) l =
    let str = Convert.ToString (i, 2)
    let x = str.PadLeft (l, '0')
    x |> Seq.map (fun ch -> if ch = '1' then true else false) |> Seq.toList

// Split the instruction into its 3 components with the format:
// (destination : int, source : int, opcode : int)
let split n =
  // Note: when extracting, position 0 = MSB (usually the rightmost bit)
  let extractBits bStart bEnd =
    let mask = ~~~(~~~0us <<< (bEnd - bStart + 1))
    (n >>> bStart) &&& mask
  
  (extractBits 10 15,   // Destination
   extractBits 5 9,     // Source
   extractBits 0 4)     // Opcode