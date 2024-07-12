import Lake
open Lake DSL

package "InterpreterBook" where
  -- add package configuration options here

lean_lib «InterpreterBook» where
  -- add library configuration options here

@[default_target]
lean_exe "interpreterbook" where
  root := `Main
