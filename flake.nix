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
      # Use my fork until https://github.com/mattwparas/helix/pull/4 is merged
      url = "github:MatrixManAtYrService/helix?ref=steel-event-system";

      # then switch to this until https://github.com/helix-editor/helix/pull/8675 is merged
      # url = "github:mattwparas/helix?ref=steel-event-system";

      # then switch to this
      # url = "github:helix-editor/helix";
    };

    code-formatter = {
      url = "github:lispunion/code-formatter";
      flake = false;
    };

  };

  outputs = { self, nixpkgs, flake-utils, steel-flake, helix-flake, code-formatter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 
        };


        # contains the fork with the plugin system
        helix = helix-flake.packages.${system}.helix;


        # Presumably the caller already has helix installed and configured to
        # their liking. Adding helix like a package would hide that one in
        # favor of the one with plugins-enabled.

        # Instead, we prepeare a second command `hxs` (helix-steel) which runs
        # the experimental version. This way, the user can use `hx` for
        # editing with something stable, and `hxs` for testing the results of
        # those edits.
        hxs = pkgs.writeShellScriptBin "hxs" ''
          exec ${helix}/bin/hx "$@"
        '';

        # the steel flake doesn't build the language server by default, add it to the package list
        steel-pkg = steel-flake.packages.${system}.steel.overrideAttrs (oldAttrs: {
          cargoBuildFlags = "-p cargo-steel-lib -p steel-interpreter -p steel-language-server";
        });

        # mattwparas used `raco fmt`, which gave me some difficuly when trying to nixify it
        # unsure if this is equivalent, but it seems close?
        code-formatter-pkg = pkgs.stdenv.mkDerivation {
          name = "code-formatter";
          src = code-formatter;
          buildInputs = with pkgs.chickenPackages_5.chickenEggs; [
            # warning: the versions of these in nixpkgs don't precisely match 
            # the ones determined by the repo but it seems to work ok
            simple-loops
            callable-data-structures
            srfi-1
            srfi-13
            matchable
          ] ++ [ pkgs.chicken ];
          installPhase = ''
            mkdir -p $out/bin
            csc -s etc.scm -j etc
            csc -s format.scm -j format
            csc -static main.scm -o $out/bin/scheme-format
          '';
        };

        helix-config = pkgs.stdenv.mkDerivation rec {
          name = "helix-config";
          src = ./.;
          phases = [ "installPhase" ];
          installPhase = ''
            mkdir $out
            cat ${pkgs.substituteAll {
              src = "${src}/languages.toml.template";
              steel_language_server_path = "${steel-pkg}/bin/steel-language-server";
              scheme_format = "${code-formatter-pkg}/bin/scheme-format";
            }} > $out/languages.toml

            cp ${src}/*.scm $out
          '';
        };

      in
      {
        packages = {
          steel = steel-pkg;
          helix-config = helix-config;
          default = helix-config;
        };

        devShells.default = pkgs.mkShell {
          packages = [ 
            steel-pkg 
            hxs 
            code-formatter-pkg
            pkgs.nixpkgs-fmt
            ];
          shellHook = ''
            export STEEL_HOME=${steel-pkg}/lib
          '';
        };
      });
}

