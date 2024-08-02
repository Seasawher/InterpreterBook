import Lake
open Lake DSL

package "InterpreterBook" where
  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

@[default_target]
lean_lib «Monkey» where
  -- add library configuration options here
  globs := #[.submodules `Monkey]
