import Std.Internal.Parsec

open Std.Internal.Parsec

namespace Parser

structure RawToken where
  data : String
  init : String.Pos
  fin : String.Pos
deriving Inhabited

structure State where
  current : String.Pos
  tokens : List RawToken
deriving Inhabited

structure Context where
  data : String

#check Lean.Parser.Category

abbrev MyParse := ReaderT Context <| StateT State String.Parser

def parseChar (c : Char) : MyParse Char := do
  let ⟨curr, tokens⟩ ← get
  let ⟨a, _⟩ ← .lift (.lift any)
  if a == c then
    let newCurr := curr + c
    let newToken : RawToken := ⟨c.toString, curr, curr⟩
    modify fun _ => ⟨newCurr, newToken :: tokens⟩
    return c
  else
    .lift <| fail ":("

def parseAny : MyParse Char := do
  let ⟨curr, tokens⟩ ← get
  let c ← .lift any
  let newCurr := curr + c
  let newToken : RawToken := ⟨c.toString, curr, curr⟩
  modify fun _ => ⟨newCurr, newToken :: tokens⟩
  return c

#check String.Pos

def tryParse (f : String.Parser α) : MyParse α := do
  let a ← f
  return a


#reduce parseChar 'c' |>.run default "cabcdef".iter

end Parser

