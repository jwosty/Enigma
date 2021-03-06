module Enigma.Assembler.Lib.Tokenizing.Tokenizer
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Enigma.Assembler.Lib.Tokenizing
open Enigma.Assembler.Lib.GeneralFunctions

// Identifies the next token, returns it (in the form of a Token), and returns the rest of the string
let takeToken input =
  let tokens = FSharpType.GetUnionCases(typeof<Token>) |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> Token)
  // Ugg, is there a more functional way to do this (without mutables)? I really hate this...
  let mutable result = None
  // Iterate over each token
  for token in tokens do
    // Match the token's regex against the string
    let matchInfo = Regex.Match(input, token.GetRegex, RegexOptions.IgnoreCase)
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

let tokenize =
  // A recursive tokenize function. prevToken is the Some of the previous token from the input (or
  // None if it was the beginning), prevTokens is the list of already analyzed tokens, and s is the
  // input string
  let rec tokenize (prevToken: Token option) (prevTokens: Token list) (s: string) =
    let token, rest = takeToken s
    let currTokens = (addIf prevTokens ((<>) Whitespaces) token)
    // If neither the previous nor the current token are separators (the string beginning counts as a
    // separator), the input is invalid
    match prevToken with
      | Some pt when not (pt.isSeparator || token.isSeparator) -> failwith "Assembly syntax error!"
      | _ -> ()
    if token = EOF then currTokens, rest else tokenize (Some(token)) currTokens rest
  (tokenize None []) >> fst