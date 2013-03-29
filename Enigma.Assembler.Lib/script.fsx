#r "FParsecCS.dll"
#r "FParsec.dll"
#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "ast.fs"
#load "parser.fs"
open System
open FParsec
open Enigma.Assembler.Parser

let doParser p str =
  match (run p str) with
    | Success (r, _, _) -> Reply r
    | Failure (msg, err, _) -> Reply (Error, messageError msg)