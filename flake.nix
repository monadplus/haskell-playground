{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
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

        defaultPackage = self.packages.${system}.default;

        devShells.default = haskellPackages.shellFor {
          packages = p: [
            p.playground
          ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            ghcid
          ];
        };

        devShell = self.devShells.${system}.default;
      });
}
