{ pkgs ? import (builtins.fetchTarball https://nixos.org/channels/nixos-19.03/nixexprs.tar.xz) {},
  ghcIde ? false
}:
let
  pkgs = import <nixpkgs> {};
  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc865;
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      stack
      ghc
    ] ++ (if ghcIde then [ghcide] else []) ;
  }
