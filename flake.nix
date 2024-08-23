{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
    steel-flake = {
      # Use my fork until https://github.com/mattwparas/steel/pull/261 is merged
      url = "github:MatrixManAtYrService/steel?ref=fix-hash";

      # then switch to this
      # url = "github:mattwparas/steel";

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

        # contains the fork with the plugin system
        helix = helix-flake.packages.${system}.helix;

        # the steel flake doesn't build the language server by default, add it to the package list
        steel-pkg = steel-flake.packages.${system}.steel.overrideAttrs (oldAttrs: {
          cargoBuildFlags = "-p cargo-steel-lib -p steel-interpreter -p steel-language-server";
        });
      in
      {

        packages = {
          steel = steel-pkg;

          # build 'languages.toml' to point at the steel language server
          helixConfig = pkgs.stdenv.mkDerivation rec {
            name = "helix-config";
            src = ./.;
            phases = [ "installPhase" ];
            installPhase = ''
              mkdir $out
              cat ${pkgs.substituteAll {
                src = "${src}/languages.toml.template";
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

