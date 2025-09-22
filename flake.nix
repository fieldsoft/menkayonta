# flake.nix - Nix Flake configuration file
# This file defines a reproducible development environment using Nix flakes
# Documentation: https://nixos.wiki/wiki/Flakes
{
  description = "Development environment with Python and Node.js";

  # Inputs are external dependencies for our flake
  inputs = {
    # nixpkgs is the main package repository for Nix
    # Using "nixos-unstable" gives us the latest packages
    # See available packages at: https://search.nixos.org/packages
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    # flake-utils helps us write flakes that work on multiple systems (Linux, macOS)
    # Documentation: https://github.com/numtide/flake-utils
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Outputs define what our flake produces
  outputs = { self, nixpkgs, flake-utils }:
    # This function creates outputs for each system (x86_64-linux, aarch64-darwin, etc.)
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Import nixpkgs for our specific system
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # devShells.default is the development environment
        # It's activated with 'nix develop' or automatically via direnv
        # Documentation: https://nixos.wiki/wiki/Development_environment_with_nix-shell
        devShells.default = pkgs.mkShell {
          # Packages to include in the shell environment
          buildInputs = with pkgs; [
            nodejs_24
            duckdb
            cargo
            cargo-tauri
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-optimize-level-2
            elmPackages.elm-review
            elmPackages.elm-test
          ];
          
          # Shell hook runs when entering the shell
          # Use this for environment setup, variables, and welcome messages
          shellHook = ''
            echo "🚀 Development environment loaded!"
            echo "📦 Node.js $(node --version)"
            echo "🦆 DuckDB $(duckdb --version)"
            echo "🌳 Elm $(elm --version)"
            echo "⛏️ Tauri $(tauri --version)"
            echo ""
          '';
          
          # Environment variables
          # These are set when the shell is active
          PROJECT_NAME = "my-awesome-project";
          NODE_ENV = "development";
        };
      });
}
