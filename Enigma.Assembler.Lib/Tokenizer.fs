
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module Enigma.Assembler.Lib.Tokenizer
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

type Token =
  | SET
  | ADD
  | SUB
  | MUL
  | DIV
  
  | JSR
  
  | RegA
  | RegB
  | RegC
  | RegX
  | RegY
  | RegZ
  | RegI
  | RegJ
  
  | LeftBracket
  | RightBracket
  
  // Returns the string regex that matches for the token
  static member getRegex token =
    match token with
      | LeftBracket -> "["
      | RightBracket -> "]"
      | _ -> string token

let isWhitespace c = c = ' ' || c = '\t'

let rec skipWhitespaces (s: string) =
  if (s.Length = 0) || not <| isWhitespace (s.[0]) then
    s
  else
    skipWhitespaces (s.[1..(s.Length - 1)])

// Scans the string until a whitespace is reached (assuming no whitespaces in the beginning!)
let rec takeToken currentToken currentChars (rest: string) =
  // Stop if either the end of the string or a whitespace has been reached
  if (rest.Length = 0) || isWhitespace (rest.[0]) then
    // We're at the end of the word; we're done here
    (currentChars, rest)
  else
    takeToken currentToken (currentChars + string rest.[0]) (rest.[1..(rest.Length - 1)])

let rec tokenize (prevTokens: string list) (s: string) =
  if s.Length = 0 then
    prevTokens
  else
    // Parse the next token, ignoring whitespaces before it
    let token, rest = skipWhitespaces s |> takeToken () ""
    tokenize (prevTokens @ [token]) rest