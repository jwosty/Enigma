#r "FParsecCS.dll"
#r "FParsec.dll"
#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "ast.fs"
#load "basics.fs"
#load "parser.fs"
open System
open FParsec
open Enigma.Assembler.Parser

let doParser p str =
  match (run p str) with
    | Success (r, _, _) -> Reply r
    | Failure (msg, err, _) -> Reply (Error, messageError msg)

// Testing case-insensitivity and whitespaces (should parse just fine)
run basicInstruction "AdD   \t  X,  \t\t   j"

// This shouldn't work (has a newline in the middle)
run basicInstruction "ADD \nA\n, \nB"

runParserOnFile dasm () "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/sample.dasm" System.Text.Encoding.Default