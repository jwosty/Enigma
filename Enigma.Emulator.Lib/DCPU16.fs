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
  let private memory = Array.zeroCreate<uint16> 0xffff // Maximum memory size for the DCPU-16
  
  let private failBadMemLoc (loc : int) = failwith <| sprintf "Bad memory location (0x%s)!" (Convert.ToString (loc, 16))
  
  let read loc =
    try
      memory.[loc]
    with :? System.IndexOutOfRangeException as e ->
      failBadMemLoc loc
  
  let write loc dat =
    try
      memory.[loc] <- dat
    with :? System.IndexOutOfRangeException as e ->
      failBadMemLoc loc