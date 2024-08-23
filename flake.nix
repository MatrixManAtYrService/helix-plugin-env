{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
    steel-flake = {
      # Use your fork until the upstream PR is merged
      url = "github:MatrixManAtYrService/steel?ref=fix-hash";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix-flake = {
      url = "github:mattwparas/helix?ref=steel-event-system";
    };
  };

  outputs = { self, nixpkgs, flake-utils, steel-flake, helix-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        steel-pkg = steel-flake.packages.${system}.steel.overrideAttrs (oldAttrs: {
          cargoBuildFlags = "-p cargo-steel-lib -p steel-interpreter -p steel-language-server";
        });
        helix = helix-flake.packages.${system}.helix;
      in
      {

        # Reference steelPackage to satisfy the linter
        packages = {
          steel = steel-pkg;
          helixConfig = pkgs.stdenv.mkDerivation rec {
            name = "helix-config";
            src = ./.;
            phases = [ "installPhase" ];
            installPhase = ''
              mkdir $out
              cat ${pkgs.substituteAll {
                src = "${src}/languages.toml.template";
                steel_formatter_path = "some-path"; # Update as needed
                steel_language_server_path = "${steel-pkg}/bin/steel-language-server";
              }} > $out/languages.toml

              # todo: make this be nonempty
              # for now: get helix to see it at all
              touch $out/helix.scm
            '';
          };

        };

        devShells.default = pkgs.mkShell {
          packages = [ helix steel-pkg ];
        };
      });
}

