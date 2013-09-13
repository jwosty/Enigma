#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "GeneralFunctions.fs"
#load "Tokenizer.fs"
open Enigma.Assembler.Lib
open Tokenizer

tokenize None [] "SET A, B"
tokenize None [] "set a, b"
tokenize None [] "sEt a, B"