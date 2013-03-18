namespace Enigma.Emulator.Lib.Instructions
open System
open Enigma.Emulator.Lib.Conversions
open Enigma.Emulator.Lib.Instructions

// Represents a DCPU-16 instruction, like:
// SET A, 0
// which can be created with:
// Instruction.Ordinary(OrdinaryOpcode.SET, A, Lit 0)
type Instruction =
  | Ordinary of OrdinaryOpcode * Register * SourceOperand
  | Special  of SpecialOpcode * Register
  
  // Get the binary representation
  member this.Bits =
    let genBits (dst : Register) (src : SourceOperand) code = src.Bits @ dst.Bits @ (Convert.ToBits code 5)
    match this with
      | Ordinary(opcode, dst, src) -> genBits dst   src   (opcode |> int)
      | Special(opcode, dst)       -> genBits dst (Lit 0) (opcode |> int)
  
  // Excecute the instruction
  member this.Eval () =
    match this with
      | Ordinary(opcode, dst, src) ->
        let mathStore op = dst.Set (op (dst.Get ()) (src.Eval ()))
        match opcode with
          | OrdinaryOpcode.SET -> dst.Set (src.Eval ())
          | OrdinaryOpcode.ADD -> mathStore op_Addition
          | OrdinaryOpcode.SUB -> mathStore op_Subtraction
          | OrdinaryOpcode.DIV -> mathStore op_Division
          | OrdinaryOpcode.MUL -> mathStore op_Multiply
          | _                  -> failwith "Ordinary opcode not implemented"
      | Special(opcode, dst)       -> failwith "Special opcode not implemented"
  
  member this.Dump =
    let bits = this.Bits
    let str = String.init (bits |> List.length) (fun i -> if bits.[i] then "1" else "0")
    Convert.ToInt32 (str, 2)
  
  // Get the raw machine code instruction, e.g:
  // SET A, 0   --->   0x8401
  // and
  // SET A, B   --->   0x0401
  member this.HexDump =
    (this.Dump.ToString ("X")).PadLeft (4, '0')


// For FSI; doesn't get compiled (#if FALSE gets rid of annoying warnings)
#if FALSE
// Use to test Instruction.Bits
SET(B, Lit 0).Bits = [true; false; false; false; false; true;    false; false; false; false; true;     false; false; false; false; true]
SET(A, Lit 1).Bits = [true; false; false; false; true; false;    false; false; false; false; false;    false; false; false; false; true]
SET(A, Reg B).Bits = [false; false; false; false; false; true;   false; false; false; false; false;    false; false; false; false; true]
// Use to test Instruction.DumpHex ()
SET(B, Lit 0).HexDump = "8421"
SET(A, Lit 1).HexDump = "8801"
SET(A, Reg B).HexDump = "0401"
#endif