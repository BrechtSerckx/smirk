{ pkgs ? import (import ./nix/sources.nix).nixpkgs { } }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # nix
    niv
    nixfmt

    # embedded
    platformio

    # ci
    act
  ];
}
