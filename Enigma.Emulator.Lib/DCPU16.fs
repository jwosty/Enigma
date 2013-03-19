module Enigma.Emulator.Lib.DCPU16
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