namespace Enigma.Assembler.Lib.Tokenizing
open Microsoft.FSharp.Reflection

type Token =
  // Registers
  | RegA | RegB | RegC
  | RegX | RegY | RegZ
  | RegI | RegJ
  // Special registers
  | SP | PC | EX

  // -----------------
  // - Basic opcodes -
  // -----------------
  | SET
  // Math
  | ADD | SUB
  | MUL | MLI | DIV | DVI
  | MOD | MDI
  // Bitwise logic
  | AND | BOR | XOR
  // Shifting
  | SHR | ASR | SHL
  // Branching
  | IFB | IFC | IFE | IFN
  | IFG | IFA | IFL | IFU
  // Other stuff
  | ADX | SBX
  | STI | STD

  // -------------------
  // - Special opcodes -
  // -------------------
  | JSR
  // Interrupt stuff
  | INT | IAG | IAS | IAQ
  // Hardware devices
  | HWN | HWQ | HWI

  // Syntax elements
  | LeftBracket
  | RightBracket
  | Comma

  // Other
  | Whitespaces
  | Newlines
  | EOF
  
  member this.Name = (fst <| FSharpValue.GetUnionFields(this, typeof<Token>)).Name

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
      | Whitespaces -> "[ \t\v]+"
      | Newlines -> "[\f\n\r]+"
      | EOF -> "$"
      // By default, the token regex is just the token name itself and not a separator 
      | _ -> this.Name

  // Tells whether or not a token is 
  member this.isSeparator =
    match this with
      | LeftBracket -> true
      | RightBracket -> true
      | Comma -> true
      | Whitespaces -> true
      | EOF -> true
      | _ -> false