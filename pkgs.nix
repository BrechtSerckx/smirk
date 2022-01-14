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
        smirk-server = superh.callCabal2nix "smirk-server" (gitignore ./server) { };
      };
    in {
      haskellPackages = super.haskellPackages.override (old: {
        overrides =
          lib.composeExtensions (old.overrides or (_: _: { })) overrides;
      });
      niv = sources.niv;
    };
  overlays = [ overlay ];
in import sources.nixpkgs { inherit overlays; }
