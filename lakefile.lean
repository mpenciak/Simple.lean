import Lake
open Lake DSL

package "simple" where
  version := v!"0.1.0"

lean_lib Simple where

@[default_target]
lean_exe "simple" where
  root := `Main
