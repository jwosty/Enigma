module Enigma.Assembler.Parser
open System
open FParsec
open Ast
open Enigma.Assembler.Basics

// A comma that can be surrounded by whitespaces
let argSep : Parser<string, unit> = ws >>. pstring "," .>> ws

let literal : Parser<_, unit> = puint16 >! (fun v -> Reply (Lit v))
let register = simpleLetterGroupSearchParser registers "Invalid register" (expected "register")

// For now, just use registers
let destinationOperand = simpleLetterGroupSearchParser registers "No such register" (expected "Destination operand")
// For now, just use registers
let sourceOperand : Parser<_,_> = attempt literal <|> (register >! (fun r -> Reply (Reg r)))

let basicOpcode = simpleLetterGroupSearchParser basicOpcodes "No such basic opcode" (expected "Two-argument opcode")
let specialOpcode = simpleLetterGroupSearchParser specialOpcodes "No such special opcode" (expected "One-argument opcode")

let basicInstruction : Parser<_,_> = (basicOpcode .>>. destinationOperand .>> argSep .>>. sourceOperand) >!
                                     (fun ((op, dst), src) -> Reply (BasicInstruction (op, dst, src)))
let specialInstruction : Parser<_,_> = (specialOpcode .>>. destinationOperand) >!
                                       (fun (op, dst) -> Reply (SpecialInstruction (op, dst)))
let instruction = ((attempt basicInstruction) <|> specialInstruction)
let dasm : Parser<_,_> =
  let instructionList = many (instruction .>> (optional newline .>> spaces))
  spaces >>. instructionList .>> spaces .>> eof