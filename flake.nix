{
  description = "agora-effect-registry";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]liqwid-nix \\e[0;5m\\]2.0.\\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/main";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };
  };

  outputs = { self, liqwid-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = liqwid-nix.allModules ++ [
        ({ self, ... }:
          {
            perSystem = { config, pkgs', self', inputs, system, ... }:
              let
                pkgs = import self.inputs.nixpkgs {
                  inherit system;
                };
              in
              {
                onchain.default = {
                  src = ./.;
                  ghc.version = "ghc925";
                  shell = { };
                  enableBuildChecks = true;
                  extraHackageDeps = [ ];
                };
                ci.required = [
                  # Currently, tests are disabled because fixture tests are difficult to run in Nix.
                  "default_build:agora-effect-registry:lib:agora-effect-registry"
                  "default_build:agora-effect-registry:lib:agora-effect-registry-server"
                  "default_build:agora-effect-registry:exe:agora-effect-registry-exe"
                  "default_haskellFormatCheck"
                  "default_cabalFormatCheck"
                ];
              };
          })
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: { };
    };
}
