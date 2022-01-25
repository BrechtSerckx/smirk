args@{ compiler ? "ghc902"}:
let pkgs = import ./pkgs.nix {inherit compiler;};
in pkgs.haskell.packages."${compiler}".shellFor {
  packages = p: [ p.smirk-server ];
  buildInputs = with pkgs; [
    # nix
    niv
    nixfmt
    # arduino
    arduino-mk
    arduino-core-unwrapped
    picocom
    # haskell
    haskellPackages.cabal-install
    haskellPackages.brittany
    haskellPackages.ghcid
    haskellPackages.hlint
  ];
  shellHook = with pkgs; ''
    cat > arduino/Base.mk << EOF
    ARDUINO_DIR = ${arduino-core-unwrapped}/share/arduino
    MONITOR_CMD = picocom
    include ${arduino-mk}/Arduino.mk
    EOF
  '';
}
