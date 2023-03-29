{
  description = "A Nix Environment for CMSC433";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";

  outputs = { self, nixpkgs }:
    let
    # Systems supported
        allSystems = [
          "x86_64-linux" # 64-bit Intel/AMD Linux
          "aarch64-linux" # 64-bit ARM Linux
          "x86_64-darwin" # 64-bit Intel macOS
          "aarch64-darwin" # 64-bit ARM macOS
        ];
        forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
            pkgs = import nixpkgs { inherit system; };
          });
    in {
    devShells = forAllSystems ({ pkgs }: {
      default = pkgs.mkShell {
      buildInputs = [ pkgs.cargo
                      pkgs.cabal-install
                      pkgs.nasm
                      pkgs.racket
                      ( pkgs.haskellPackages.ghcWithPackages (p: with p; [ HUnit ]) )
                      pkgs.ocaml
                      pkgs.ocamlPackages.findlib
                      pkgs.ocamlPackages.utop
                      pkgs.pandoc
                      pkgs.rustc
                      pkgs.rsync
                      pkgs.texlive.combined.scheme-full
                      pkgs.zip
                    ];
    };
    });
  };
}
