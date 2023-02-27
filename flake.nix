{
  description = "agora-effect-registry";

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=a2494bf2042d605ca1c4a679401bdc4971da54fb";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.3.0";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };

    liqwid-libs.url = "github:Liqwid-Labs/liqwid-libs";
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest {
            inherit system;
          };
        in
        {
          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskell.packages.ghc924.fourmolu_0_9_0_0;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = { };
            enableBuildChecks = true;
            extraHackageDeps = [
              "${inputs.liqwid-libs}/liqwid-plutarch-extra"
              "${inputs.liqwid-libs.inputs.ply}/ply-core"
              "${inputs.liqwid-libs.inputs.ply}/ply-plutarch"
              "${inputs.liqwid-libs}/plutarch-quickcheck"
            ];
          };
          ci.required = [
            # Currently, tests are disabled because fixture tests are difficult to run in Nix.
            "build:agora-effect-registry:lib:agora-effect-registry"
            "build:agora-effect-registry:lib:agora-effect-registry-server"
            "build:agora-effect-registry:exe:agora-effect-registry-exe"
            "haskellFormatCheck"
            "cabalFormatCheck"
          ];
        };

      flake.hydraJobs.x86_64-linux = (
        self.checks.x86_64-linux
        // self.packages.x86_64-linux
      );
    };
}
