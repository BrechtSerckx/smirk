let
  sources = import ./nix/sources.nix;
  overlay = self: super:
    let
      hlib = super.haskell.lib;
      lib = super.lib;
      sources = import nix/sources.nix;
      gitignore = path:
        super.nix-gitignore.gitignoreSourcePure [ (path + /.gitignore) ] path;
    in {
      niv = sources.niv;
    };
  overlays = [ overlay ];
in import sources.nixpkgs { inherit overlays; }
