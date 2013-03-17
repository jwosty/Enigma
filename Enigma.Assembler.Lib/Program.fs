
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module DCPU16.Assembler.Main

open System

let eval x =
  let lexbuf = Lexing.LexBuffer<char>.FromString x
  
  while not lexbuf.IsPastEndOfStream do
    printfn "%A" (Lexer.tokenize lexbuf)

[<EntryPoint>]
let rec main args =
  printf "> "
  eval (Console.ReadLine ())
  main args