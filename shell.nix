{ }:
let pkgs = import ./pkgs.nix;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    # nix
    niv
    nixfmt
    # arduino
    arduino-mk
    arduino-core-unwrapped
    picocom
  ];
  shellHook = with pkgs; ''
    cat > arduino/Base.mk << EOF
    ARDUINO_DIR = ${arduino-core-unwrapped}/share/arduino
    MONITOR_CMD = picocom
    include ${arduino-mk}/Arduino.mk
    EOF
  '';
}
