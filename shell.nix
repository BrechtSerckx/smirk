let
  project = import ./default.nix;
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in project.shellFor {
  packages = ps: with ps; [ smirk-captain ];

  withHoogle = true;

  tools = {
    cabal = "3.6.2.0";
    hlint = "3.5";
    ghcid = "0.8.7";
    ormolu = "0.5.0.1";
    haskell-language-server = "1.9.1.0";
    cabal-fmt = "0.1.6";
  };

  buildInputs = with pkgs; [
    # nix
    niv
    nixfmt

    # embedded
    platformio

    # ci
    act
  ];

  exactDeps = true;
}
