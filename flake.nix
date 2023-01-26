{
  description = "A Nix Environment for CMSC433";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
        ghcPkgs = pkgs.haskellPackages.ghcWithPackages (p: with p; [
          HUnit
        ]);
    in {

    devShell.x86_64-linux = pkgs.mkShell {
      buildInputs = [ pkgs.cargo
                      pkgs.nasm
                      pkgs.racket
                      ghcPkgs
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
  };
}
