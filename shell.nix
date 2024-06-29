let
  pkgs = import <nixpkgs> { };

  ghciwatch-compat = builtins.getFlake "github:evanrelf/ghciwatch-compat";

  fsm-playground = pkgs.haskellPackages.callCabal2nix "fsm-playground" ./. { };

in
pkgs.mkShell {
  inputsFrom = [ fsm-playground.env ];

  packages = [
    ghciwatch-compat.packages.${builtins.currentSystem}.ghciwatch-compat-ghcid
    pkgs.cabal-install
  ];
}
