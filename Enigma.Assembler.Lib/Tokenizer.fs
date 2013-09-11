
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module Enigma.Assembler.Lib.Tokenizer
open System.Text.RegularExpressions

module Tokens =
  type BasicOpcode =
    | SET = 0x01
    | ADD = 0x02
    | SUB = 0x03
    | MUL = 0x04
    | DIV = 0x06
  type SpecialOpcode =
    | JSR = 0x01
  type Register =
    | RegA = 0x00
    | RegB = 0x01
    | RegC = 0x02
    | RegX = 0x03
    | RegY = 0x04
    | RegZ = 0x05
    | RegI = 0x06
    | RegJ = 0x07
  type DestinationOperand =
    | Reg of Register
    | Label of string
  type SourceOperand =
    | Lit of uint16
    | Reg of Register
    | Label of string

let opcodeChars = ['a'..'z'] @ ['A'..'Z']
let literalChars = ['0'..'9']
let whitespaceChars = [' '; '\t']

let isWhitespace c = c = ' ' || c = '\t'

let rec skipWhitespaces (s: string) =
  if not <| isWhitespace (s.[0]) then
    s
  else
    skipWhitespaces (s.[1..(s.Length - 1)])

// Scans the string until a whitespace is reached (assuming no whitespaces in the beginning!)
let rec takeToken currentToken (rest: string) =
  if isWhitespace (rest.[0]) then
    // We're at the end of the word; we're done here
    (currentToken, rest)
  else
    takeUntilToken (currentToken + string rest.[0]) (rest.[1..(rest.Length - 1)])