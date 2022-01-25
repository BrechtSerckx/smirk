{ compiler }:
let
  sources = import ./nix/sources.nix;
  overlay = self: super:
    let
      hlib = super.haskell.lib;
      lib = super.lib;
      sources = import nix/sources.nix;
      gitignore = path:
        super.nix-gitignore.gitignoreSourcePure [ (path + /.gitignore) ] path;
      overrides = selfh: superh: {
        smirk-server =
          superh.callCabal2nix "smirk-server" (gitignore ./server) { };
        # for bytestring constraint
        serialport =
          hlib.dontCheck (superh.callHackage "serialport" "0.5.1" { });
        # for ghc > 9
        capability =
          superh.callHackage "capability" "0.5.0.0" { };
        # for capability above
        lens =
          superh.callHackage "lens" "5.0.1" {};
      };
    in {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages."${compiler}".override (old: {
            overrides =
              lib.composeExtensions (old.overrides or (_: _: { })) overrides;
          });
        };
      };
      niv = sources.niv;
    };
  overlays = [ overlay ];
in import sources.nixpkgs { inherit overlays; }
