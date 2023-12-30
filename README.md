# README

Which haskell packages are available?

```bash
$ nix repl
nix-repl> :lf .
nix-repl> inputs.nixpkgs.legacyPackages.x86_64-linux.haskellPackages.*
```
