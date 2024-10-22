{
  description = "Formula 1 Pools Management";

  # This is a flake reference to the stacklock2nix repo.
  #
  # Note that if you copy the `./flake.lock` to your own repo, you'll likely
  # want to update the commit that this stacklock2nix reference points to:
  #
  # $ nix flake lock --update-input stacklock2nix
  #
  # You may also want to lock stacklock2nix to a specific release:
  #
  # inputs.stacklock2nix.url = "github:cdepillabout/stacklock2nix/v1.5.0";
  inputs.stacklock2nix.url = "github:cdepillabout/stacklock2nix/main";

  # This is a flake reference to Nixpkgs.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs, stacklock2nix }:
    let
      # System types to support.
      supportedSystems = [
        "x86_64-linux"
      ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor =
        forAllSystems (system: import nixpkgs { inherit system; overlays = [ stacklock2nix.overlay self.overlays.default ]; });
    in
    {
      # A Nixpkgs overlay.
      overlays.default = final: prev: {
        # This is a top-level attribute that contains the result from calling
        # stacklock2nix.
        f1pools-stacklock = final.stacklock2nix {
          stackYaml = ./stack.yaml;

          # The Haskell package set to use as a base.  You should change this
          # based on the compiler version from the resolver in your stack.yaml.
          baseHaskellPkgSet = final.haskell.packages.ghc98;

          # Any additional Haskell package overrides you may want to add.
          additionalHaskellPkgSetOverrides = hfinal: hprev: {
            mkDerivation = args:
              hprev.mkDerivation
              (args // {
                doCheck = false;
              } //
                nixpkgs.lib.optionalAttrs (args.isExecutable or false) { enableSeparateBinOutput = true; }
              );
            # The servant-cassava.cabal file is malformed on GitHub:
            # https://github.com/haskell-servant/servant-cassava/pull/29
            servant-cassava =
              final.haskell.lib.compose.overrideCabal
                { editedCabalFile = null; revision = null; }
                hprev.servant-cassava;
            ansi-terminal =
              final.haskell.lib.compose.overrideCabal
                { enableSeparateBinOutput = false; }
                hprev.ansi-terminal;
            async =
              final.haskell.lib.compose.overrideCabal
                { enableSeparateBinOutput = false; }
                hprev.async;
            http2 =
              final.haskell.lib.compose.overrideCabal
                { enableSeparateBinOutput = false; }
                hprev.http2;
            prettyprinter =
              final.haskell.lib.compose.overrideCabal
                { enableSeparateBinOutput = false; }
                hprev.prettyprinter;
            wai-extra =
              final.haskell.lib.compose.overrideCabal
                { enableSeparateBinOutput = false; }
                hprev.wai-extra;

            postgresql-libpq-configure =
              final.haskell.lib.compose.addBuildTool
                final.postgresql_15
                hprev.postgresql-libpq-configure;
          };

          # Additional packages that should be available for development.
          additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
            # Some Haskell tools (like cabal-install and ghcid) can be taken from the
            # top-level of Nixpkgs.
            final.cabal-install
            final.ghcid
            final.stack
            final.pkg-config
            # Some Haskell tools need to have been compiled with the same compiler
            # you used to define your stacklock2nix Haskell package set.  Be
            # careful not to pull these packages from your stacklock2nix Haskell
            # package set, since transitive dependency versions may have been
            # carefully setup in Nixpkgs so that the tool will compile, and your
            # stacklock2nix Haskell package set will likely contain different
            # versions.
            final.haskell.packages.ghc98.haskell-language-server
            # Other Haskell tools may need to be taken from the stacklock2nix
            # Haskell package set, and compiled with the example same dependency
            # versions your project depends on.
            #stacklockHaskellPkgSet.some-haskell-lib
            final.zlib
            final.postgresql_15
          ];

          # When creating your own Haskell package set from the stacklock2nix
          # output, you may need to specify a newer all-cabal-hashes.
          all-cabal-hashes = final.fetchFromGitHub {
            owner = "commercialhaskell";
            repo = "all-cabal-hashes";
            rev = "663142756f7a4a61efde65479a688a6a09ee067a";
            sha256 = "0ybxsx7yh8q60vdh6c95nqpsycs8yw34qk86iip10scj73xsjg1a";
          };
        };

        # One of our local packages.
        f1pools = final.f1pools-stacklock.pkgSet.f1pools;

        # You can also easily create a development shell for hacking on your local
        # packages with `cabal`.
        f1pools-dev-shell = final.f1pools-stacklock.devShell;
      };

      packages = forAllSystems (system: rec {
        f1pools = nixpkgsFor.${system}.f1pools;
        default = f1pools;
      });

      devShells = forAllSystems (system: rec {
        f1pools-dev-shell = nixpkgsFor.${system}.f1pools-dev-shell;
        default = f1pools-dev-shell;
      });
    };
}
