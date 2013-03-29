#r "FParsecCS.dll"
#r "FParsec.dll"
#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "ast.fs"
#load "parser.fs"
open System
open FParsec
open Enigma.Assembler.Parser

run basicOpcode "SET"
run destinationOperand "C"