#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "parsing/AbstractSyntaxTree.fs"
#load "GeneralFunctions.fs"
#load "tokenizing/Tokens.fs"
#load "tokenizing/Tokenizer.fs"
#load "parsing/Parser.fs"
open Enigma.Assembler.Lib.GeneralFunctions
open Enigma.Assembler.Lib.Tokenizing
open Enigma.Assembler.Lib.Tokenizing.Tokenizer
open Enigma.Assembler.Lib.Parsing
open Enigma.Assembler.Lib.Parsing.Parser

tokenize None [] "SET A, B"
tokenize None [] "set a, b"
tokenize None [] "sEt a, B"

match [] with
  | [42] -> Some 42
  | [] -> None
  | _ -> Some 0