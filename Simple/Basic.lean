namespace Simple

inductive Tp
  | num
  | str
  | bool
  | fn (argTps : List Tp) (outTp : Tp)
  | list (tp : Tp)
  | struct (structRef :

inductive Term
  | var (name : String)
  | num (val : Nat)
  | str (val : String)
  | bool (val : Bool)
  | app (fn : Term) (args : List Term)
  | fn (params : List String) (argTps : List Tp) (retTp : Tp) (body : Term)
  | ite (cond : Term) (thenBranch : Term) (elseBranch : Term)
  | letIn (name : String) (tp : Tp) (val : Term) (body : Term)
  | loop (cond : Term) (body : Term)
  | list (elements : List Term) (elemTp : Tp)

end Simple

