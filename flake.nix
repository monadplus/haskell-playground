{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks, gitignore, ... }:
    let
      lib = nixpkgs.lib;

      defaultSystems = with flake-utils.lib.system; [
        x86_64-linux
        # x86_64-darwin
        # aarch64-linux
        # aarch64-darwin
      ];
    in
    flake-utils.lib.eachSystem defaultSystems (system:
      let
        inherit (gitignore.lib) gitignoreSource;

        overlay = self: super: {
          playground =
            self.callCabal2nix "playground" (gitignoreSource ./cabal) {
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
