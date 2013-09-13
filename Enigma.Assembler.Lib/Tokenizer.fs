
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module Enigma.Assembler.Lib.Tokenizer
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Enigma.Assembler.Lib
open GeneralFunctions

type Token =
  // Registers and the like
  | RegA
  | RegB
  | RegC
  | RegX
  | RegY
  | RegZ
  | RegI
  | RegJ
  
  // Basic opcodes
  | SET
  | ADD
  | SUB
  | MUL
  | DIV
  
  // Special opcodes
  | JSR
  
  // Syntax elements
  | LeftBracket
  | RightBracket
  | Comma
  
  // Other
  | Whitespaces
  | EOF
  
  // Returns a tuple containing the token's string regex and whether or not the token is a type of
  // separator (for tokens that aren't, then two or more successive instances of these tokens must
  // be separated with one or more separating tokens)  
  member this.GetRegex =
    match this with
      | RegA -> "A"
      | RegB -> "B"
      | RegC -> "C"
      | RegX -> "X"
      | RegY -> "Y"
      | RegZ -> "Z"
      | RegI -> "I"
      | RegJ -> "J"
      | LeftBracket -> "\["
      | RightBracket -> "\]"
      | Comma -> "\,"
      | Whitespaces -> "\s+"
      | EOF -> "$"
      // By default, the token regex is just the token name itself and not a separator 
      | _ -> this.Name
  
  member this.isSeparator =
    match this with
      | LeftBracket -> true
      | RightBracket -> true
      | Comma -> true
      | Whitespaces -> true
      | EOF -> true
      | _ -> false
    
  member this.Name = (fst <| FSharpValue.GetUnionFields(this, typeof<Token>)).Name

// Identifies the next token, returns it (in the form of a Token), and returns the rest of the string
let takeToken input =
  let tokens = FSharpType.GetUnionCases(typeof<Token>) |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Token)
  // Ugg, is there a more functional way to do this (without mutables)? I really hate this...
  let mutable result = None
  // Iterate over each token
  for token in tokens do
    // Match the token's regex against the string
    let matchInfo = Regex.Match(input, token.GetRegex)
    // If the match is a success and at the beginning of the string, then we've found a valid token
    if matchInfo.Success then
      let capture = matchInfo.Captures.[0]
      if capture.Index = 0 then
        // But wait! If the previous token wasn't a separator and this one isn't either, that's bad!
        // Example: "JSRA" is invalid -- it needs a separator token to tokenize right, like "JSR A"
        result <- Some(token, input.[(capture.Length)..(input.Length - 1)])
  match result with
    | Some r -> r
    // If no tokens matched against the input, we've stumbled upon a syntax error!
    | None -> failwith "Assembly syntax error!"

let rec tokenize (prevTokens: Token list) (s: string) =
  let token, rest = takeToken s
  if token
  let currTokens = (addIf prevTokens ((<>) Whitespaces) token)
  if token = EOF then currTokens, rest else tokenize currTokens rest