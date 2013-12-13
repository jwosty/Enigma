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

tokenize "SET A, B"
tokenize "set a, b"
tokenize "sEt a, B"

let buildCode = tokenize >> parse

buildCode "set a, b"
buildCode "mul b, a"
buildCode "add i, j\nsub j, i"