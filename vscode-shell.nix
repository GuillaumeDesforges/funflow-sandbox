let
    shell = import ./shell.nix;
in
    shell {
        enableGhcide = true;
    }