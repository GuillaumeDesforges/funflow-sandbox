{ enableGhcide ? false
}:
let
  pkgs = import <nixpkgs> {};
  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc865;
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      stack
      ghc
      docker
    ] ++ lib.optionals enableGhcide [ghcide];
  }
