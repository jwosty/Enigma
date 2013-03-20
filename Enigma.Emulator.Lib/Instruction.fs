namespace Enigma.Emulator.Lib.Instructions
open System
open Enigma.Emulator.Lib.Conversions
open Enigma.Emulator.Lib.Instructions

open Ops

// Represents a DCPU-16 instruction, like:
// SET A, 0
// which can be created with:
// Instruction.Ordinary(OrdinaryOpcode.SET, A, Lit 0)
type Instruction =
  | Ordinary of OrdinaryOpcode * Register * SourceOperand
  | Special  of SpecialOpcode * Register
  
  // Get the binary representation
  member this.Bits =
    let genBits (dst : Register) (src : SourceOperand) code = src.Bits @ (bits dst) @ (Convert.ToBits code 5)
    match this with
      | Ordinary(opcode, dst, src) -> genBits dst   src   (opcode |> int)
      | Special(opcode, dst)       -> genBits dst (Lit 0us) (opcode |> int)
  
  // Excecute the instruction
  member this.Eval () =
    match this with
      | Ordinary(opcode, dst, src) ->
        let mathStore op = setReg dst (op (getReg dst) (src.Eval ()))
        match opcode with
          | OrdinaryOpcode.SET -> setReg dst (src.Eval ())
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
  
  // Load the machine code instruction n
  static member LoadMachineCode n =
    let srcInf, valDst, valOpcode = split n
    let valSrc = srcInf &&& 0b011111 // The absolute destination
    let src =
      if (srcInf >>> 5) = 1 then // If the first bit is set
        Lit(valSrc - 1 |> uint16) // destination is a literal
      else
        Reg(enum<Register> valSrc) // destination is a register
    let dst = enum<Register> valDst
    let opcode = enum<OrdinaryOpcode> valOpcode
    // For now, just assume it's a basic instruction
    Ordinary(opcode, dst, src)

// For FSI; doesn't get compiled (#if FALSE gets rid of annoying warnings)
#if FALSE
// Use to test Instruction.Bits
Ordinary(OrdinaryOpcode.SET, Register.B, Lit 0us).Bits = [true; false; false; false; false; true;    false; false; false; false; true;     false; false; false; false; true]
Ordinary(OrdinaryOpcode.SET, Register.A, Lit 1us).Bits = [true; false; false; false; true; false;    false; false; false; false; false;    false; false; false; false; true]
Ordinary(OrdinaryOpcode.SET, Register.A, Reg Register.B).Bits = [false; false; false; false; false; true;   false; false; false; false; false;    false; false; false; false; true]
// Use to test Instruction.DumpHex ()
Ordinary(OrdinaryOpcode.SET, Register.B, Lit 0us).HexDump = "8421"
Ordinary(OrdinaryOpcode.SET, Register.A, Lit 1us).HexDump = "8801"
Ordinary(OrdinaryOpcode.SET, Register.A, Reg Register.B).HexDump = "0401"
#endif