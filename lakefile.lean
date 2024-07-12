import Lake
open Lake DSL

package "InterpreterBook" where
  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

@[default_target]
lean_lib «InterpreterBook» where
  -- add library configuration options here
  globs := #[.submodules `InterpreterBook]
