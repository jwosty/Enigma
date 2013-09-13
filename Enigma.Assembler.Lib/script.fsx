#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "GeneralFunctions.fs"
#load "Tokenizer.fs"
#load "AbstractSyntaxTree.fs"
#load "Parser.fs"
open Enigma.Assembler.Lib
open Tokenizer
open GeneralFunctions
open AbstractSyntaxTree
open Parser

tokenize None [] "SET A, B"
tokenize None [] "set a, b"
tokenize None [] "sEt a, B"