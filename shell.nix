with import <nixpkgs> {};

let ghc = (import (builtins.fetchGit {
      name = "nixos-20.09";
      url = https://github.com/nixos/nixpkgs-channels/;
      ref = "refs/heads/nixos-20.09";
    }) {}).haskell.compiler.ghc884;
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
    ];
  }
