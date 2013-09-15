namespace Enigma.Assembler.Lib.Tokenizing
open System

[<AttributeUsage(AttributeTargets.Property)>]
type TokenRegexAttribute(name) =
  inherit Attribute()
  member val private name = name