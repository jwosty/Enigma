module Enigma.Emulator.Lib.Conversions
open System

type System.Convert with
  static member ToBits (i : int) l =
    let str = Convert.ToString (i, 2)
    let x = str.PadLeft (l, '0')
    x |> Seq.map (fun ch -> if ch = '1' then true else false) |> Seq.toList