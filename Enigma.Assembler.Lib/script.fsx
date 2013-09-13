#I "/Users/alanwostenberg/Projects/FSharp/Enigma/Enigma.Assembler.Lib/"
#load "GeneralFunctions.fs"
#load "Tokenizer.fs"
open Enigma.Assembler.Lib
open Tokenizer

Tokenizer.tokenize [] "SET A, B"