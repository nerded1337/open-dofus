with import <nixpkgs> {};

let ghc = (import (builtins.fetchGit {
      name = "nixos-20.03";
      url = https://github.com/nixos/nixpkgs-channels/;
      ref = "refs/heads/nixos-20.03";
      rev = "4dc8447c55fe5262bad1019121a8e6d3f9e1e71f";
    }) {}).haskell.compiler.ghc883;
in
  haskell.lib.buildStackProject {
    name = "myEnv";
    inherit ghc;
    buildInputs = [
      git
      zlib
      postgresql
      gobject-introspection
      systemd
      llvm_7
    ];
  }
