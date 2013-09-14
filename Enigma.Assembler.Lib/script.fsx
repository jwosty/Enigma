#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "AbstractSyntaxTree.fs"
#load "GeneralFunctions.fs"
#load "Tokens.fs"
#load "Tokenizer.fs"
#load "Parser.fs"
open Enigma.Assembler.Lib.GeneralFunctions
open Enigma.Assembler.Lib.Tokenizing.Tokenizer
open Enigma.Assembler.Lib.Parsing.Parser

tokenize None [] "SET A, B"
tokenize None [] "set a, b"
tokenize None [] "sEt a, B"