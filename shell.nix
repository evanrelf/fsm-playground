let
  pkgs = import <nixpkgs> { };

  ghciwatch-compat = builtins.getFlake "github:evanrelf/ghciwatch-compat";

in
pkgs.mkShell {
  packages = [
    ghciwatch-compat.packages.${builtins.currentSystem}.ghciwatch-compat-ghcid
    (pkgs.ghc.withPackages (p: with p; [
      aeson
      algebraic-graphs
      hashable
      template-haskell
    ]))
  ];
}
