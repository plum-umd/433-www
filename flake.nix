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
    in
  {
    devShells = forAllSystems ({ pkgs }: {
      default = pkgs.mkShell {
        packages = (with pkgs; [
          cargo
          cabal-install
          nasm
          racket
          (let ghcPkgs = pkgs.haskellPackages.ghcWithPackages (p: with p; [
            HUnit
          ]); in ghcPkgs)
          ocaml
          ocamlPackages.findlib
          ocamlPackages.utop
          pandoc
          rustc
          rsync
          texlive.combined.scheme-full
          zip
        ]); }; });
    };
}
