{
  description = "Formula 1 Pools Management";

  # This is a flake reference to Nixpkgs.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      # System types to support.
      supportedSystems = [
        "x86_64-linux"
      ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor =
        forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; });

      overlay = final: prev: {
        myBasePkgSet = final.haskell.packages.ghc98;

        myHaskellPkgs = final.myBasePkgSet.override {
          overrides = hfinal: hprev: {
            f1pools = hfinal.callCabal2nix "f1pools" ./. {};
          };
        };


        # Our local packages
        f1pools = final.myHaskellPkgs.f1pools;

        # Just the exe.
        f1pools-just-static = final.haskell.lib.compose.justStaticExecutables final.f1pools;

        # You can also easily create a development shell for hacking on your local
        # packages with `cabal`.
        f1pools-dev-shell = final.myHaskellPkgs.shellFor {
          packages = ps: [ps.f1pools];

          nativeBuildInputs = [
            final.cabal-install
            final.postgresql
            final.ghcid

            final.myBasePkgSet.haskell-language-server
          ];
        };
      };
    in
    {
      # A Nixpkgs overlay.
      overlays.default = overlay;

      packages = forAllSystems (system: rec {
        f1pools = nixpkgsFor.${system}.f1pools-just-static;
        default = f1pools;
      });

      devShells = forAllSystems (system: rec {
        f1pools-dev-shell = nixpkgsFor.${system}.f1pools-dev-shell;
        default = f1pools-dev-shell;
      });
    };
}
