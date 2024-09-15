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

    emacs-overlay.url = "github:nix-community/emacs-overlay";

  };

  outputs = { self, nixpkgs, flake-utils, steel-flake, helix-flake, emacs-overlay}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 
          overlays = [emacs-overlay.overlays.default];
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

        scmfmt = pkgs.chickenPackages_5.chickenEggs.scmfmt;

        helix-config = pkgs.stdenv.mkDerivation rec {
          name = "helix-config";
          src = ./.;
          phases = [ "installPhase" ];
          installPhase = ''
            mkdir $out
            cat ${pkgs.substituteAll {
              src = "${src}/languages.toml.template";
              steel_language_server_path = "${steel-pkg}/bin/steel-language-server";
              scmfmt = "${scmfmt}/bin/scmfmt";
            }} > $out/languages.toml

            cp ${src}/helix.scm $out
            cp ${src}/init.scm $out

            mkdir $out/steel_home
          '';
        };

        emacs = pkgs.emacsWithPackagesFromUsePackage {
          config = ./emacs.el;
          package = pkgs.emacs-unstable;
          extraEmacsPackages = epkgs: [
            epkgs.symex
            epkgs.evil
            epkgs.paredit
          ];
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
            scmfmt
            pkgs.nixpkgs-fmt
            emacs
            ];
          shellHook = ''
            export STEEL_HOME=$PWD/.steel
          '';
        };
      });
}

