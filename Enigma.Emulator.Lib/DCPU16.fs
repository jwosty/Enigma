module Enigma.Emulator.Lib.DCPU16
open System
open System.Collections.Generic

// Readable/writeable register memory
module Registers =
  let private registers =
    let tmpd = new Dictionary<_,_>()
    let dictAdd (k, v) = tmpd.Add (k, v)
    [
      "A", 0us; "B", 0us; "C", 0us;
      "X", 0us; "Y", 0us; "Z", 0us;
      "I", 0us; "J", 0us;
    ] |> List.iter (dictAdd)
    tmpd
  
  let get reg = if registers.ContainsKey reg then registers.[reg] else failwith "No such register exists"
  
  let set reg value = if registers.ContainsKey reg then registers.[reg] <- value else failwith "No such register exists"

module RAM =
  let private memory = Array.zeroCreate<uint16> 0x10000 // Maximum memory size for the DCPU-16
  
  // Since loc is only an unsigned 16-bit integer, it's impossible to access an invalid memory location -- last valid is 0xffff
  let read (loc : uint16) =
    memory.[loc |> int]
  
  // Since loc is only an unsigned 16-bit integer, it's impossible to access an invalid memory location -- last valid is 0xffff
  let write (loc : uint16) dat =
    memory.[loc |> int] <- dat