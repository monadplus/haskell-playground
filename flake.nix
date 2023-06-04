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
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ghc928 = super.haskell.packages.ghc928.override (old: {
                overrides =
                  let
                    oldOverrides = old.overrides or (_: _: { });

                    manualOverrides = haskPkgsNew: haskPkgsOld:
                      {
                        playground =
                          haskPkgsOld.playground.override {
                            transformers = haskPkgsNew.transformers_0_6_1_0;
                          };
                      };

                    packageSources =
                      self.haskell.lib.packageSourceOverrides {
                        playground = gitignoreSource ./cabal;
                      };

                  in
                  self.lib.fold self.lib.composeExtensions oldOverrides [
                    packageSources
                    manualOverrides
                  ];
              });
            };
          };
        };

        config.allowBroken = true;
        pkgs = import nixpkgs { inherit config system; overlays = [ overlay ]; };
        haskellPackages = pkgs.haskell.packages.ghc928;
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
            pkgs.nixpkgs-fmt
            ormolu
            cabal-fmt
            ghcid
            hlint
          ];
        };

        # nix check
        # nix build .#checks.x86_64-linux.pre-commit-check
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
