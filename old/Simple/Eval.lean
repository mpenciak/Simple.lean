import Std.Data.HashMap
import Std.Data.TreeMap
import Simple.Basic

namespace Simple

open Std

mutual

structure Environment where
  bindings : List (String × Value)

inductive Value
  | num (val : Nat)
  | str (val : String)
  | bool (val : Bool)
  | closure (params : List String) (body : Term) (env : Environment)
  | list (elements : List Value) (elemTp : Tp)

end

def Environment.lookUp (env : Environment) (name : String) : Option Value :=
  lookupAux env.bindings
  where lookupAux (bindings : List (String × Value)) :=
    match bindings with
    | (valName, val) :: rest => if valName == name then val else lookupAux rest
    | [] => none

mutual

partial def eval (env : Environment) : Term → Option Value
  | Term.var name => env.lookUp name
  | Term.num n => some (Value.num n)
  | Term.str s => some (Value.str s)
  | Term.bool b => some (Value.bool b)
  | Term.fn params _argTps _retTp body => some (Value.closure params body env)
  | Term.list elements elemTp => do
      let mut values := []
      for elem in elements do
        let val ← eval env elem
        values := values ++ [val]
      return Value.list values elemTp
  | Term.app fn args => do
      let fnVal ← eval env fn
      match fnVal with
      | Value.closure params body closureEnv => do
          let mut argVals := []
          for arg in args do
            let val ← eval env arg
            argVals := argVals ++ [val]
          if params.length != argVals.length then
            none
          else
            let newEnv := params.zip argVals ++ closureEnv.bindings
            eval ⟨newEnv⟩ body
      | _ => none
  | Term.ite cond thenBranch elseBranch => do
      let condVal ← eval env cond
      match condVal with
      | Value.bool true => eval env thenBranch
      | Value.bool false => eval env elseBranch
      | _ => none
  | Term.letIn name _tp val body => do
      let valResult ← eval env val
      let newEnv := (name, valResult) :: env.bindings
      eval ⟨newEnv⟩ body
  | Term.loop cond body => evalLoop env cond body

partial def evalLoop (env : Environment) (cond : Term) (body : Term) : Option Value := do
  let condVal ← eval env cond
  match condVal with
  | Value.bool true =>
      let _ ← eval env body
      evalLoop env cond body
  | Value.bool false =>
      some (Value.bool true)
  | _ => none

end

end Simple
