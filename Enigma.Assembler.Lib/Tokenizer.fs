
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
  
  // ... yeah
  | Whitespaces
  
  // Returns a tuple containing the token's string regex and whether or not the token is a type of
  // separator (for tokens that aren't, then two or more successive instances of these tokens must
  // be separated with one or more separating tokens)  
  static member GetRegexAndInfo token =
    match token with
      | RegA -> ("A", false)
      | RegB -> ("B", false)
      | RegC -> ("C", false)
      | RegX -> ("X", false)
      | RegY -> ("Y", false)
      | RegZ -> ("Z", false)
      | RegI -> ("I", false)
      | RegJ -> ("J", false)
      | LeftBracket -> ("\[", true)
      | RightBracket -> ("\]", true)
      | Comma -> ("\,", true)
      | Whitespaces -> ("\s+", true)
      // By default, the token regex is just the token name itself and not a separator 
      | _ -> (token.Name, false)
  
  member this.Name = (fst <| FSharpValue.GetUnionFields(this, typeof<Token>)).Name

// Identifies the next token, returns it (in the form of a Token), and returns the rest of the string
let takeToken input =
  let tokens = FSharpType.GetUnionCases(typeof<Token>) |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Token)
  let mutable result = None
  for token in tokens do
    let matchInfo = Regex.Match(input, fst <| Token.GetRegexAndInfo(token))
    if matchInfo.Success then
      let capture = matchInfo.Captures.[0]
      if capture.Index = 0 then
        result <- Some(token, input.[(capture.Length)..(input.Length - 1)])
  match result with
    | Some r -> r
    | None -> failwith "Assembly syntax error!"

let skipWhitespaces (s: string) =
  let token, rest = takeToken s
  if (token = Whitespaces) then rest else s

let rec tokenize (prevTokens: Token list) (s: string) =
  let token, rest = takeToken s
  (addIf prevTokens ((<>) Whitespaces) token), rest
  (*
  let rest = skipWhitespaces s
  if rest.Length = 0 then
    prevTokens
  else
    // Parse the next token, ignoring whitespaces before it
    let token, rest = takeToken rest
    tokenize (prevTokens @ [token]) rest
  *)