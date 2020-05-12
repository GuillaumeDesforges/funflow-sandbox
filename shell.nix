let
  pkgs = import <nixpkgs> {};

  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc865;

in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      stack
      ghcide
    ];
  }