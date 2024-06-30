{
  description = "fsm-playground";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    ghciwatch-compat = {
      url = "github:evanrelf/ghciwatch-compat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, inputs', pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            localSystem = system;
            overlays = [
              inputs.ghciwatch-compat.overlays.default
            ];
          };

        devShells.default =
          let
            fsm-playground =
              pkgs.haskellPackages.callCabal2nix "fsm-playground" ./. { };
          in
          pkgs.mkShell {
            inputsFrom = [ fsm-playground.env ];
            packages = [
              pkgs.cabal-install
              pkgs.ghciwatch-compat-ghcid
              pkgs.graphviz
            ];
          };
      };
    };
}
