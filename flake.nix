{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/x86_64-linux";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils = {
      url = "github:numtide/flake-utils";
      # Now eachDefaultSystem is only using ["x86_64-linux"]
      inputs.systems.follows = "systems";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: super: {
          playground =
            self.callCabal2nix "playground" ./cabal {
              transformers = self.transformers_0_6_1_0;
            };
        };

        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.extend overlay;
      in
      {
        packages = {
          inherit (haskellPackages) playground;

          default = haskellPackages.playground;
        };

        devShells.default = haskellPackages.shellFor {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          packages = p: [
            p.playground
          ];

          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            ghcid
          ];
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              ormolu.enable = true;
              cabal-fmt.enable = true;
            };
          };
        };
      }
    );
}
