let sources = import ./nix/sources.nix;
in { pkgs ? import sources.nixpkgs { } }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    sources.niv
    nixfmt
    arduino-mk
    arduino-core-unwrapped
    picocom
  ];
  shellHook = with pkgs; ''
    cat > Base.mk << EOF
    ARDUINO_DIR = ${arduino-core-unwrapped}/share/arduino
    MONITOR_CMD = picocom
    include ${arduino-mk}/Arduino.mk
    EOF
  '';
}
