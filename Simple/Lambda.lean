import Lean

/-!
In this file we do some very simple syntax parsing and delaborating for fun
-/

namespace Lambda

section Types

inductive Expr
  | var (n : Nat)
  | lam (body : Expr)
  | app (func arg : Expr)
deriving Repr

end Types

section Parser

open Lean Parser in
def lambdaUnicodeParser : Parser := unicodeSymbol "λ" "l"

declare_syntax_cat lambda_term

syntax (name := lambdaBVar) ":" noWs num : lambda_term

syntax (name := lambdaApp) "(" lambda_term ws lambda_term ")" : lambda_term

syntax (name := lambdaLam) group("(" noWs lambdaUnicodeParser noWs ".") lambda_term ")" : lambda_term

syntax (name := lambdaTerm) "⟦" lambda_term "⟧" : term

end Parser

section Elab

open Lean Elab Term Meta

def bVarElab : TermElab := fun stx _ => do
  return ← mkAppM ``Expr.var #[mkNatLit stx.toNat]

mutual

partial def appElab (func arg : TSyntax `lambda_term) : TermElabM Lean.Expr := do
  let funcExpr ← exprElab func none
  let argExpr ← exprElab arg none
  return ← mkAppM ``Expr.app #[funcExpr, argExpr]

partial def lamElab : TermElab := fun stx _ => do
  let bodyExpr ← exprElab stx none
  return ← mkAppM ``Expr.lam #[bodyExpr]

partial def exprElab : TermElab
  | `(lambda_term| :$n:num) => bVarElab n
  | `(lambda_term| (λ. $b:lambda_term)) => lamElab b
  | `(lambda_term| ($f:lambda_term $a:lambda_term)) => fun _ => appElab f a
  | _ => fun _ => throwUnsupportedSyntax

end

@[term_elab lambdaTerm]
def elabToTerm : TermElab
  | `(⟦$x:lambda_term⟧) => exprElab x
  | _ => fun _ => throwUnsupportedSyntax

open PrettyPrinter Delaborator

@[app_unexpander Lambda.Expr.var]
def unexpandLambdaVar : Unexpander
  | `($(_) $n:num) => `(⟦:$n⟧)
  | _ => pure

@[app_unexpander Lambda.Expr.lam]
def unexpandLambdaLam : Unexpander
  | `($(_) ⟦$b⟧) => `(⟦(λ.$b)⟧)
  | _ => pure

@[app_unexpander Lambda.Expr.app]
def unexpandLambdaApp : Unexpander
  | `($(_) ⟦$a⟧ ⟦$b⟧) => `(⟦($a $b)⟧)
  | _ => pure

end Elab

end Lambda
