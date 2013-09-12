
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module Enigma.Assembler.Lib.Tokenizer
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

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
  
  // Returns the string regex that matches for the token
  static member GetRegex token =
    match token with
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
      | _ -> token.Name
  
  member this.Name = (fst <| FSharpValue.GetUnionFields(this, typeof<Token>)).Name

let isWhitespace c = c = ' ' || c = '\t'

let rec skipWhitespaces (s: string) =
  if (s.Length = 0) || not <| isWhitespace (s.[0]) then
    s
  else
    skipWhitespaces (s.[1..(s.Length - 1)])

// Identifies the next token, returns it (in the form of a Token), and returns the rest of the string
let takeToken input =
  let tokens = FSharpType.GetUnionCases(typeof<Token>) |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Token)
  let mutable result = None
  for token in tokens do
    let matchInfo = Regex.Match(input, Token.GetRegex(token))
    if matchInfo.Success then
      let capture = matchInfo.Captures.[0]
      if capture.Index = 0 then
        result <- Some(token, input.[(capture.Length)..(input.Length - 1)])
  match result with
    | Some r -> r
    | None -> failwith "Assembly syntax error!"

let rec tokenize (prevTokens: Token list) (s: string) =
  let rest = skipWhitespaces s
  if rest.Length = 0 then
    prevTokens
  else
    // Parse the next token, ignoring whitespaces before it
    let token, rest = takeToken rest
    tokenize (prevTokens @ [token]) rest